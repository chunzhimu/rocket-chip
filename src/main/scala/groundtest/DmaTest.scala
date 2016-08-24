package groundtest

import Chisel._
import dma._
import rocket.{TLBPTWIO, HasCoreParameters}
import uncore.tilelink._
import uncore.agents.CacheBlockBytes
import junctions.Timer
import cde.Parameters

class VirtualMemAdapter(implicit val p: Parameters) extends Module
    with HasCoreParameters
    with HasTileLinkParameters {
  val io = new Bundle {
    val vmem = new ClientUncachedTileLinkIO().flip
    val pmem = new ClientUncachedTileLinkIO
    val ptw  = new TLBPTWIO
  }

  val tlBlockOffset = tlBeatAddrBits + tlByteAddrBits

  val cur_vpn = Reg(UInt(width = vpnBits))
  val cur_ppn = Reg(UInt(width = ppnBits))
  val trans_valid = Reg(init = Bool(false))
  val trans_inflight = Reg(init = Bool(false))

  val req_vaddr = io.vmem.acquire.bits.full_addr()
  val req_vpn = req_vaddr(paddrBits - 1, pgIdxBits)
  val req_idx = req_vaddr(pgIdxBits - 1, 0)
  val req_paddr = Cat(cur_ppn, req_idx)
  val req_block = req_paddr(paddrBits - 1, tlBlockOffset)

  val req_ok = trans_valid && cur_vpn === req_vpn

  io.pmem.acquire.valid := io.vmem.acquire.valid && req_ok
  io.vmem.acquire.ready := io.pmem.acquire.ready && req_ok
  io.pmem.acquire.bits := io.vmem.acquire.bits
  io.pmem.acquire.bits.addr_block := req_block
  io.vmem.grant <> io.pmem.grant

  io.ptw.req.valid := io.vmem.acquire.valid && !req_ok && !trans_inflight
  io.ptw.req.bits.prv := UInt(0)
  io.ptw.req.bits.pum := Bool(false)
  io.ptw.req.bits.mxr := Bool(false)
  io.ptw.req.bits.store := Bool(true)
  io.ptw.req.bits.fetch := Bool(false)
  io.ptw.req.bits.addr := req_vpn

  when (io.ptw.req.fire()) {
    trans_valid := Bool(false)
    trans_inflight := Bool(true)
    cur_vpn := req_vpn
  }

  when (io.ptw.resp.valid) {
    trans_valid := Bool(true)
    trans_inflight := Bool(false)
    cur_ppn := io.ptw.resp.bits.pte.ppn
  }

  assert(!io.ptw.resp.valid || io.ptw.resp.bits.pte.leaf(),
         "page table lookup is invalid")
}

class DmaTestDriver(start: BigInt, nBlocks: Int)
                   (implicit val p: Parameters) extends Module
    with HasTileLinkParameters {
  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val dma = new ClientDmaIO
    val busy = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }

  val nBytesPerBlock = tlDataBeats * tlDataBits / 8
  val nBytes = nBlocks * nBytesPerBlock

  val addr_block = Reg(UInt(width = tlBlockAddrBits))
  val blocks_left = Reg(UInt(width = log2Up(nBlocks)))
  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && io.mem.acquire.bits.hasMultibeatData(), tlDataBeats)
  val (get_beat, get_done) = Counter(
    io.mem.grant.fire() && io.mem.grant.bits.hasMultibeatData(), tlDataBeats)

  val put_data = Cat(addr_block, put_beat)
  val get_data = Cat(addr_block - UInt(nBlocks), get_beat)

  val (s_idle :: s_put_req :: s_put_resp ::
       s_dma_req :: s_dma_resp :: s_wait ::
       s_get_req :: s_get_resp :: s_done :: Nil) = Enum(Bits(), 9)
  val state = Reg(init = s_idle)

  io.mem.acquire.valid := (state === s_put_req) || (state === s_get_req)
  io.mem.acquire.bits := Mux(state === s_put_req,
    PutBlock(
      client_xact_id = UInt(0),
      addr_block = addr_block,
      addr_beat = put_beat,
      data = put_data),
    GetBlock(
      client_xact_id = UInt(0),
      addr_block = addr_block))
  io.mem.grant.ready := (state === s_put_resp) || (state === s_get_resp)

  io.dma.req.valid := (state === s_dma_req)
  io.dma.req.bits := ClientDmaRequest(
    cmd = DmaRequest.DMA_CMD_COPY,
    src_start = UInt(start),
    dst_start = UInt(start + nBytes),
    segment_size = UInt(nBytes))

  io.finished := (state === s_done)

  when (state === s_idle) {
    addr_block := UInt(start)
    blocks_left := UInt(nBlocks - 1)
    state := s_put_req
  }
  when (put_done) {
    state := s_put_resp
  }
  when (state === s_put_resp && io.mem.grant.valid) {
    when (blocks_left === UInt(0)) {
      state := s_dma_req
    } .otherwise {
      blocks_left := blocks_left - UInt(1)
      addr_block := addr_block + UInt(1)
      state := s_put_req
    }
  }
  when (io.dma.req.fire()) { state := s_wait }
  when (state === s_dma_resp && io.dma.resp.valid) { state := s_wait }
  when (state === s_wait && !io.busy) {
    addr_block := UInt(start + nBlocks)
    blocks_left := UInt(nBlocks - 1)
    state := s_get_req
  }
  when (state === s_get_req && io.mem.acquire.ready) {
    state := s_get_resp
  }
  when (get_done) {
    when (blocks_left === UInt(0)) {
      state := s_done
    } .otherwise {
      blocks_left := blocks_left - UInt(1)
      addr_block := addr_block + UInt(1)
      state := s_get_req
    }
  }

  assert(state =/= s_get_resp || !io.mem.grant.valid ||
         io.mem.grant.bits.data === get_data,
         "DmaTest: get data does not match")
}

class DmaTest(implicit p: Parameters) extends GroundTest()(p) {
  val pageBlocks = (1 << pgIdxBits) / p(CacheBlockBytes)
  val driver = Module(new DmaTestDriver(0, 2 * pageBlocks))
  val adapter = Module(new VirtualMemAdapter)
  val frontend = Module(new DmaFrontend)
  val backend = Module(new DmaBackend)

  require(io.ptw.size == 2)
  require(io.mem.size == 2)
  require(io.cache.size == 0)

  io.ptw(0) <> adapter.io.ptw
  io.ptw(1) <> frontend.io.ptw
  io.mem(0) <> adapter.io.pmem
  io.mem(1) <> backend.io.mem
  io.status.finished := driver.io.finished
  io.status.timeout.valid := Bool(false)
  io.status.error.valid := Bool(false)

  driver.io.busy := frontend.io.busy
  adapter.io.vmem <> driver.io.mem
  frontend.io.cpu <> driver.io.dma
  backend.io.dma <> frontend.io.dma
}
