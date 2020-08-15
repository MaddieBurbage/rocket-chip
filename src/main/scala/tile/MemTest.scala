// Test of the mem interface
// (c) Maddie Burbage, August 2020, for the Bailey Research Group at Williams

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket.{HellaCacheReq} //For cache accesses

// Wrapper for the accelerator
class MemTest()(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new MemTestImp(this)
}

// Main accelerator class
class MemTestImp(outer: MemTest)(implicit p: Parameters) extends LazyRoCCModule(outer) {
  // Basic state
  val s_idle :: s_busy :: s_wait :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle) //Idle until handling an instruction
  io.interrupt := Bool(false)

  // Instruction inputs
  val address = Reg(init = 0.U(64.W))
  val data = Reg(init = 0.U(64.W))
  val typ = Reg(init = 0.U(2.W)) //Can be 3 wide but we don't have room in inst
  val command = Reg(init = 0.U(5.W))
  val rd = Reg(init = 0.U(5.W))
  val returned = Reg(init = 0.U(64.W))

  //RoCC interface
  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)

  //Memory request interface
  io.mem.req.valid := (state === s_busy)
  io.busy := (state === s_busy) || (state === s_wait)
  io.mem.req.bits.addr := address
  io.mem.req.bits.tag :=  address(5,0)
  io.mem.req.bits.cmd := command
  io.mem.req.bits.data := data
  io.mem.req.bits.typ := typ
  io.mem.req.bits.phys := Bool(false)

  //Response interface
  io.resp.bits.data := returned
  io.resp.bits.rd := rd

  when(io.cmd.fire()) {
    state := s_busy

    address := io.cmd.bits.rs1
    data := io.cmd.bits.rs2
    typ := io.cmd.bits.inst.funct(6,5)
    command := io.cmd.bits.inst.funct(4,0)
    rd := io.cmd.bits.inst.rd
  }

  when(io.mem.req.fire()) {
    state := s_wait
  }

  when(io.mem.resp.valid) {
    state := s_resp
    assert(io.mem.resp.bits.addr === address)
    assert(io.mem.resp.bits.tag === address(5,0))

    returned := io.mem.resp.bits.data
  }

  when(io.resp.fire()) {
    state := s_idle
  }

}
