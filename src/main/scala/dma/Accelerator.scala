package dma

import Chisel._
import rocket.RoCC
import uncore.tilelink._
import cde.{Parameters, Field}

case object CopyAccelShareMemChannel extends Field[Boolean]

class CopyAccelerator(implicit p: Parameters) extends RoCC()(p) {
  val ctrl = Module(new DmaController)
  val backend = Module(new DmaBackend)

  ctrl.io.cmd <> io.cmd
  io.resp <> ctrl.io.resp
  io.ptw.head <> ctrl.io.ptw
  io.busy := ctrl.io.busy

  backend.io.dma <> ctrl.io.dma

  if (p(CopyAccelShareMemChannel)) {
    require(io.utl.size == 0)
    io.autl <> backend.io.mem
  } else {
    require(io.utl.size == 1)
    io.utl.head <> backend.io.mem
    io.autl.acquire.valid := Bool(false)
    io.autl.grant.ready := Bool(false)
  }
  io.mem.req.valid := Bool(false)
  io.interrupt := Bool(false)
}
