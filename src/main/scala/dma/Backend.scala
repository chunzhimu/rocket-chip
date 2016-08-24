package dma

import Chisel._
import cde.{Parameters, Field}
import junctions._
import junctions.NastiConstants._
import uncore.tilelink._
import uncore.util._

case object NDmaTransactors extends Field[Int]
case object NDmaXacts extends Field[Int]

trait HasDmaParameters {
  implicit val p: Parameters
  val nDmaTransactors = p(NDmaTransactors)
  val nDmaXacts = p(NDmaXacts)
  val dmaXactIdBits = log2Up(nDmaXacts)
  val addrBits = p(PAddrBits)
  val dmaStatusBits = 2
  val trackerMemXacts = 4
}

abstract class DmaModule(implicit val p: Parameters) extends Module with HasDmaParameters
abstract class DmaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasDmaParameters

class DmaRequest(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(width = dmaXactIdBits)
  val cmd = UInt(width = DmaRequest.DMA_CMD_SZ)
  val source = UInt(width = addrBits)
  val dest = UInt(width = addrBits)
  val length = UInt(width = addrBits)

  def isPrefetch(dummy: Int = 0): Bool =
    cmd === DmaRequest.DMA_CMD_PFR || cmd === DmaRequest.DMA_CMD_PFW
}

class DmaResponse(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(width = dmaXactIdBits)
  val status = UInt(width = dmaStatusBits)
}

object DmaRequest {
  val DMA_CMD_SZ = 2

  val DMA_CMD_COPY = UInt("b00")
  val DMA_CMD_PFR  = UInt("b10")
  val DMA_CMD_PFW  = UInt("b11")

  def apply(xact_id: UInt = UInt(0),
            cmd: UInt,
            source: UInt,
            dest: UInt,
            length: UInt)(implicit p: Parameters): DmaRequest = {
    val req = Wire(new DmaRequest)
    req.xact_id := xact_id
    req.cmd := cmd
    req.source := source
    req.dest := dest
    req.length := length
    req
  }
}
import DmaRequest._

class DmaIO(implicit p: Parameters) extends DmaBundle()(p) {
  val req = Decoupled(new DmaRequest)
  val resp = Decoupled(new DmaResponse).flip
}

class DmaTrackerIO(implicit p: Parameters) extends DmaBundle()(p) {
  val dma = (new DmaIO).flip
  val mem = new ClientUncachedTileLinkIO
}

class DmaTrackerFile(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val dma = (new DmaIO).flip
    val mem = Vec(nDmaTransactors, new ClientUncachedTileLinkIO)
  }

  val trackers = List.fill(nDmaTransactors) { Module(new DmaTracker) }
  val reqReadys = trackers.map(_.io.dma.req.ready).asUInt

  io.mem <> trackers.map(_.io.mem)

  if (nDmaTransactors > 1) {
    val resp_arb = Module(new RRArbiter(new DmaResponse, nDmaTransactors))
    resp_arb.io.in <> trackers.map(_.io.dma.resp)
    io.dma.resp <> resp_arb.io.out

    val selection = PriorityEncoder(reqReadys)
    trackers.zipWithIndex.foreach { case (tracker, i) =>
      tracker.io.dma.req.valid := io.dma.req.valid && selection === UInt(i)
      tracker.io.dma.req.bits := io.dma.req.bits
    }
    io.dma.req.ready := reqReadys.orR
  } else {
    trackers.head.io.dma <> io.dma
  }
}

class DmaTracker(implicit p: Parameters) extends DmaModule()(p)
    with HasTileLinkParameters with HasNastiParameters {
  val io = new DmaTrackerIO

  private val blockOffset = tlBeatAddrBits + tlByteAddrBits
  private val blockBytes = tlDataBeats * tlDataBytes

  val data_buffer = Reg(Vec(2 * tlDataBeats, Bits(width = tlDataBits)))
  val get_inflight = Reg(UInt(width = 2 * tlDataBeats))
  val put_inflight = Reg(Bool())
  val put_half = Reg(UInt(width = 1))
  val get_half = Reg(UInt(width = 1))
  val prefetch_put = Reg(Bool())
  val get_done = !get_inflight.orR

  val src_block = Reg(UInt(width = tlBlockAddrBits))
  val dst_block = Reg(UInt(width = tlBlockAddrBits))
  val offset    = Reg(UInt(width = blockOffset))
  val alignment = Reg(UInt(width = blockOffset))
  val shift_dir = Reg(Bool())

  val bytes_left = Reg(UInt(width = addrBits))

  val acq = io.mem.acquire.bits
  val gnt = io.mem.grant.bits

  val (s_idle :: s_get :: s_put :: s_prefetch ::
       s_wait :: s_resp :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && acq.hasData(), tlDataBeats)

  val put_mask = Seq.tabulate(tlDataBytes) { i =>
    val byte_index = Cat(put_beat, UInt(i, tlByteAddrBits))
    byte_index >= offset && byte_index < bytes_left
  }.asUInt

  val prefetch_sent = io.mem.acquire.fire() && io.mem.acquire.bits.isPrefetch()
  val prefetch_busy = Reg(init = UInt(0, trackerMemXacts))
  val (prefetch_id, _) = Counter(prefetch_sent, trackerMemXacts)

  val base_index = Cat(put_half, put_beat)
  val put_data = Wire(init = Bits(0, tlDataBits))
  val beat_align = alignment(blockOffset - 1, tlByteAddrBits)
  val bit_align = Cat(alignment(tlByteAddrBits - 1, 0), UInt(0, 3))
  val rev_align = UInt(tlDataBits) - bit_align

  def getBit(value: UInt, sel: UInt): Bool =
    (value >> sel)(0)

  when (alignment === UInt(0)) {
    put_data := data_buffer(base_index)
  } .elsewhen (shift_dir) {
    val shift_index = base_index - beat_align
    when (bit_align === UInt(0)) {
      put_data := data_buffer(shift_index)
    } .otherwise {
      val upper_bits = data_buffer(shift_index)
      val lower_bits = data_buffer(shift_index - UInt(1))
      val upper_shifted = upper_bits << bit_align
      val lower_shifted = lower_bits >> rev_align
      put_data := upper_shifted | lower_shifted
    }
  } .otherwise {
    val shift_index = base_index + beat_align
    when (bit_align === UInt(0)) {
      put_data := data_buffer(shift_index)
    } .otherwise {
      val upper_bits = data_buffer(shift_index + UInt(1))
      val lower_bits = data_buffer(shift_index)
      val upper_shifted = upper_bits << rev_align
      val lower_shifted = lower_bits >> bit_align
      put_data := upper_shifted | lower_shifted
    }
  }

  val put_acquire = PutBlock(
    client_xact_id = UInt(2),
    addr_block = dst_block,
    addr_beat = put_beat,
    data = put_data,
    wmask = Some(put_mask))

  val get_acquire = GetBlock(
    client_xact_id = get_half,
    addr_block = src_block,
    alloc = Bool(false))

  val prefetch_acquire = Mux(prefetch_put,
    PutPrefetch(client_xact_id = prefetch_id, addr_block = dst_block),
    GetPrefetch(client_xact_id = prefetch_id, addr_block = dst_block))

  val resp_xact_id = Reg(UInt(width = dmaXactIdBits))

  io.mem.acquire.valid := (state === s_get) ||
                          (state === s_put && get_done) ||
                          (state === s_prefetch && !prefetch_busy(prefetch_id))
  io.mem.acquire.bits := MuxLookup(
    state, prefetch_acquire, Seq(
      s_get -> get_acquire,
      s_put -> put_acquire))
  io.mem.grant.ready := Bool(true)
  io.dma.req.ready := state === s_idle
  io.dma.resp.valid := state === s_resp
  io.dma.resp.bits.xact_id := resp_xact_id
  io.dma.resp.bits.status := UInt(0)

  when (io.dma.req.fire()) {
    val src_off = io.dma.req.bits.source(blockOffset - 1, 0)
    val dst_off = io.dma.req.bits.dest(blockOffset - 1, 0)
    val direction = src_off < dst_off

    resp_xact_id := io.dma.req.bits.xact_id
    src_block := io.dma.req.bits.source(addrBits - 1, blockOffset)
    dst_block := io.dma.req.bits.dest(addrBits - 1, blockOffset)
    alignment := Mux(direction, dst_off - src_off, src_off - dst_off)
    shift_dir := direction
    offset := dst_off
    bytes_left := io.dma.req.bits.length + dst_off
    get_inflight := UInt(0)
    put_inflight := Bool(false)
    get_half := UInt(0)
    put_half := UInt(0)

    when (io.dma.req.bits.cmd === DMA_CMD_COPY) {
      state := s_get
    } .elsewhen (io.dma.req.bits.isPrefetch()) {
      prefetch_put := io.dma.req.bits.cmd(0)
      state := s_prefetch
    } .otherwise {
      assert(Bool(false), "Unknown DMA command")
    }
  }

  when (state === s_get && io.mem.acquire.ready) {
    get_inflight := get_inflight | FillInterleaved(tlDataBeats, UIntToOH(get_half))
    src_block := src_block + UInt(1)
    val bytes_in_buffer = UInt(blockBytes) - alignment
    val extra_read = alignment > UInt(0) && !shift_dir && // dst_off < src_off
                     get_half === UInt(0) && // this is the first block
                     bytes_in_buffer < bytes_left // there is still more data left to fetch
    get_half := get_half + UInt(1)
    when (!extra_read) { state := s_put }
  }

  when (prefetch_sent) {
    prefetch_busy := prefetch_busy | UIntToOH(prefetch_id)
    when (bytes_left < UInt(blockBytes)) {
      bytes_left := UInt(0)
      state := s_wait
    } .otherwise {
      bytes_left := bytes_left - UInt(blockBytes)
      dst_block := dst_block + UInt(1)
    }
  }

  when (io.mem.grant.fire()) {
    when (gnt.g_type === Grant.prefetchAckType) {
      prefetch_busy := prefetch_busy & ~UIntToOH(gnt.client_xact_id)
    } .elsewhen (gnt.hasData()) {
      val write_half = gnt.client_xact_id(0)
      val write_idx = Cat(write_half, gnt.addr_beat)
      get_inflight := get_inflight & ~UIntToOH(write_idx)
      data_buffer(write_idx) := gnt.data
    } .otherwise {
      put_inflight := Bool(false)
    }
  }

  when (put_done) { // state === s_put
    put_half := put_half + UInt(1)
    offset := UInt(0)
    when (bytes_left < UInt(blockBytes)) {
      bytes_left := UInt(0)
    } .otherwise {
      bytes_left := bytes_left - UInt(blockBytes)
    }
    put_inflight := Bool(true)
    dst_block := dst_block + UInt(1)
    state := s_wait
  }

  when (state === s_wait && get_done && !put_inflight && !prefetch_busy.orR) {
    state := Mux(bytes_left === UInt(0), s_resp, s_get)
  }

  when (io.dma.resp.fire()) { state := s_idle }
}

class DmaBackend(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val dma = (new DmaIO).flip
    val mem = new ClientUncachedTileLinkIO
  }

  val memArb = Module(new ClientUncachedTileLinkIOArbiter(nDmaTransactors))
  val trackerFile = Module(new DmaTrackerFile)
  
  trackerFile.io.dma <> io.dma
  memArb.io.in <> trackerFile.io.mem
  io.mem <> memArb.io.out
}
