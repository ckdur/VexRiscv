package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.sram._

object Axi4SharedToSRAM{
  /**
    * Return the axi and sram configuration
    */
  def getConfigs(addressAxiWidth: Int, addressBRAMWidth: Int, dataWidth: Int, idWidth: Int): (Axi4Config, SRAMConfig) =
    (
      Axi4Config(
        addressWidth = addressAxiWidth,
        dataWidth    = dataWidth,
        idWidth      = idWidth,
        useLock      = false,
        useRegion    = false,
        useCache     = false,
        useProt      = false,
        useQos       = false
      ),
      SRAMConfig(
        dataWidth    = dataWidth,
        addressWidth = addressBRAMWidth
      )
    )

  def main(args: Array[String]) {
    SpinalVhdl(new Axi4SharedToSRAM(32,13,32, 4).setDefinitionName("TopLevel"))
  }
}

case class Axi4SharedToSRAM(addressAxiWidth: Int, addressSRAMWidth: Int, dataWidth: Int, idWidth: Int) extends Component{
  val (axiConfig, sramConfig) = Axi4SharedToSRAM.getConfigs(addressAxiWidth, addressSRAMWidth, dataWidth, idWidth)

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sram = master(SRAM(sramConfig))
  }

  val wordRange = addressSRAMWidth + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)

  val arw = io.axi.arw.unburstify
  val stage0 = arw.haltWhen(arw.write && !io.axi.writeData.valid)
  io.axi.readRsp.data := io.sram.rddata
  io.sram.addr := stage0.addr(axiConfig.wordRange).resized
  io.sram.wrdata := io.axi.writeData.data
  io.sram.en := stage0.fire
  io.sram.we := stage0.write
  io.sram.wstrb := io.axi.writeData.strb

  io.axi.writeData.ready :=  arw.valid && arw.write  && stage0.ready

  val stage1 = stage0.stage
  stage1.ready := (io.axi.readRsp.ready && !stage1.write) || ((io.axi.writeRsp.ready || ! stage1.last) && stage1.write)

  io.axi.readRsp.valid  := stage1.valid && !stage1.write
  io.axi.readRsp.id  := stage1.id
  io.axi.readRsp.last := stage1.last
  io.axi.readRsp.setOKAY()
  if(axiConfig.useRUser) io.axi.readRsp.user  := stage1.user

  io.axi.writeRsp.valid := stage1.valid &&  stage1.write && stage1.last
  io.axi.writeRsp.setOKAY()
  io.axi.writeRsp.id := stage1.id
  if(axiConfig.useBUser) io.axi.writeRsp.user := stage1.user

  io.axi.arw.ready.noBackendCombMerge //Verilator perf
}
