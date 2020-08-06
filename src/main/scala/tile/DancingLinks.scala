// Accelerator for Dancing Links manipulations
// (c) Maddie Burbage, August 2020, for the Bailey Research Group at Williams

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket.{HellaCacheReq} //For cache accesses

// Wrapper for the accelerator
class DancingLinks()(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new DancingLinksImp(this)
}

// Main accelerator class
class DancingLinksImp(outer: DancingLinks)(implicit p: Parameters) extends LazyRoCCModule(outer) {

  //State control
  val memState = DancingLinksImp.memFSM(io.mem.req.fire())
  val doneWithMem = io.mem.resp.valid && (memState === DancingLinksImp.m_storeSecond)
  val dancingFired = Wire(Bool())
  val state = DancingLinksImp.FSM(dancingFired, doneWithMem, io.resp.fire())

  //RoCC Interface
  io.interrupt := Bool(false)
  io.cmd.ready := state === DancingLinksImp.s_idle
  io.busy := state === DancingLinksImp.s_busy
  io.resp.valid := state === DancingLinksImp.s_resp
  io.resp.bits.data := 0.U //No rd needed yet
  io.resp.bits.rd := 0.U


  // Instruction data
  val setupInst = io.cmd.bits.inst.funct(1).asBool() //Setup or Dancing function
  dancingFired := io.cmd.fire() & !setupInst
  val cover = Reg(init = Bool(true)) // Whether to cover or uncover the element

  // Setup data
  val nameArray = Reg(init = 0.U(64.W))
  val structWidth = Reg(init = 16.U(8.W)) //Bytes of width for the element struct

  // Dancing Link data
  val elementIndex = Reg(init = io.cmd.bits.rs1) //Index to current struct

  // Instruction arrives
  when(io.cmd.fire()) {
    when(setupInst) { //Setting up the accelerator
      nameArray := io.cmd.bits.rs1
      structWidth := io.cmd.bits.rs2
    } .otherwise { //Covering or uncovering an element
      elementIndex := io.cmd.bits.rs1
      cover := io.cmd.bits.inst.funct(0) === 0.U //Function bit 0 controls uncovering/covering
    }
  }

  // First element's addresses
  val offsetAddress = nameArray + 8.U //Base address, aligned to skip a name pointer

  // Read address:
  val elementAddress = offsetAddress + elementIndex * structWidth

  val elementSecond = elementAddress + 4.U //Indices are ints, so skipping one is 4 bytes

  // Neighbor elements' addresses
  val firstNeighborIndex = Reg(init = 0.U(32.W))
  val secondNeighborIndex = Reg(init = 0.U(32.W))
  val firstNeighborAddress = offsetAddress + firstNeighborIndex * structWidth
  val secondNeighborAddress = offsetAddress + secondNeighborIndex * structWidth

  // Write addresses:
  val firstNeighborChange = firstNeighborAddress + 4.U //Change 2nd index
  val secondNeighborChange = secondNeighborAddress + 0.U //Change 1st index

  // mem interface setup
  io.mem.req.valid := canRequest && (mem_state != m_none)
  io.mem.req.bits.phys := Bool(false)

  when(memState === DancingLinksImp.m_load) { //Read from the element
    io.mem.req.bits.cmd := 0.U
    io.mem.req.bits.addr := elementAddress
    io.mem.req.bits.tag := elementAddress(5,0)
    io.mem.req.bits.typ := 3.U //For 64-bit width of response (read 2 ints)

    when(io.mem.resp.valid) {
      firstNeighborIndex := io.mem.resp.data(31, 0)
      secondNeighborIndex := io.mem.resp.data(63, 32)
    }
  } .otherwise { //Write to a neighbor's fields
    io.mem.req.bits.cmd := 1.U
    io.mem.req.bits.typ := 2.U //For 32-bit width of write (write 1 int)

    when(memState === DancingLinksImp.m_storeFirst) { //Change first neighbor
      io.mem.req.bits.addr := firstNeighborChange
      io.mem.req.bits.data := firstNeighborIndex
      io.mem.req.bits.tag := firstNeighborChange(5,0)
    } .elsewhen(memState === DancingLinksImp.m_storeSecond) { //Change second neighbor
      io.mem.req.bits.addr := secondNeighborChange
      io.mem.req.bits.data := secondNeighborIndex
      io.mem.req.bits.tag := secondNeighborChange(5,0)
    }
  }

}

object DancingLinksImp {
  val s_idle :: s_busy :: s_resp :: Nil = Enum(Bits(), 3)
  val m_load :: m_storeFirst :: m_storeSecond :: Nil = Enum(Bits(), 3)

  def FSM(gotInst: UInt, finishedMem: UInt, sentResp: UInt) {
    // Accelerator states
    val state = Reg(init = DancingLinksImp.s_idle) //Idle until handling an instruction

    //State transitions
    when(gotInst) {
      state := DancingLinksImp.s_busy
    }

    when(finishedMem) {
      state := DancingLinksImp.s_resp
    }

    when(sentResp) {
      state := DancingLinksImp.s_idle
    }

    state
  }

  def memFSM(sentReq: UInt, gotResp: UInt) {
    // Memory states
    val mem_state = Reg(init = DancingLinksImp.m_none)

    when(sentReq) {
      when(mem_state === m_storeFirst) {
        mem_state := m_storeSecond
      }
    }

    when(gotResp) {
      when(mem_state === m_load) {
        mem_state := m_storeFirst
      } .elsewhen(mem_state === m_storeSecond) {
        mem_state := m_load
      }
    }

    mem_state
  }
}
