package me.arcticlight.launchconn

import javax.sound.midi._
import scala.util.Try

object LaunchConn {
  private var deviceList: Array[MidiDevice] = _
  private var onHit: ((Int, Boolean) => Unit) = _

  def initLaunchConn(): Unit = {
    if(deviceList != null) closeLaunchConn()
    deviceList = for(deviceInfo <- MidiSystem.getMidiDeviceInfo.filter(_.getName.contains("Launchpad"))) yield {
      println(s"LaunchConn: Attempting to attach to MIDI device ${deviceInfo.getName}")
      val device = MidiSystem.getMidiDevice(deviceInfo)
      Try({
        device.open()
        device.getTransmitter
      }).map(_.setReceiver(LaunchRcv))
      device
    }
    if(deviceList.length == 0) System.err.println("LaunchConn: Did not connect to Launchpad controller. Is something wrong?")
  }

  def closeLaunchConn(): Unit = {
    deviceList.foreach(x => {
      if(x.isOpen) x.close()
    })
    deviceList = null
    onHit = null
  }

  def setHitHandler(handler: (Int, Boolean) => Unit): Unit = {
    this.onHit = handler
  }

  def sendRawMessage(messageType: Int, d1: Int, d2: Int): Unit = {
    val v = new ShortMessage()
    v.setMessage(messageType, d1, d2)
    deviceList.foreach(x=>Try(x.getReceiver.send(v,-1)))
  }

  def resetConnectedLaunchpad(): Unit = {
    sendRawMessage(176, 0, 0)
  }

  private def calcVelocity(red: Short, green: Short): Short = {
    require(red >= 0 && green >= 0 && red <= 3 && green <= 3, "LaunchConn: Red and Green colors must be in range [0-3]")
    ((16*green) + red + 12).toShort
  }

  def setGridLight(lightNo: Short, red: Short, green: Short): Unit = {
    sendRawMessage(144, lightNo, calcVelocity(red, green))
  }

  def setControlLight(lightNo: Short, red: Short, green: Short): Unit = {
    sendRawMessage(176, 104 + lightNo, calcVelocity(red, green))
  }

  private object LaunchRcv extends Receiver {
    override def send(message: MidiMessage, timeStamp: Long): Unit = {
      message match {
        case x: ShortMessage =>
          if (onHit != null) {
            onHit(x.getData1, x.getData2 != 0)
          } else {
            System.err.println(s"LaunchConn: Unhandled MIDI message ${x.getCommand} ${x.getData1} ${x.getData2}")
          }
        case _ => System.err.println("LaunchConn: Unhandled MIDI message <Unrecognized Message>")
      }
    }
    override def close(): Unit = {}
  }
}