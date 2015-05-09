package ghpages.examples

import japgolly.scalajs.react._, vdom.all._
import scala.scalajs.js
import ghpages.examples.util.SideBySide
import org.scalajs.dom.console

import scala.util.Random

/** Scala version of "A Stateful Component" on http://facebook.github.io/react/ */
object TimerExample {

  def content = Timer()

  def randId:String = {
    val uuid = java.util.UUID.randomUUID().toString()
    uuid
  }

  object Param {
    val SeimakoDay = 30
    val KomakoWeight = 1
    val SeimakoWeight = 30
    val MaxmakoMaleWeight = 60
    val MaxmakoFemaleWeight = 40
    val WeightPercentEsa = 0.6
    val MaxMakoDay = 120
    // %
    val WeightPercentPerInverseDay = 1
    // kg (1/day*2)*this =>
    val EsaPerWeight = 0.05
    // kg
    val MilkPerWeight = 0.05
    // kg
    val ValuePerMilkWeight = 100
    // yen
    val ValuePerMakoWeight = 200
    // yen
    val ValuePerEsaWeight = 150
    // yen
    val FixedCostBase = 1000
    val TaxRate = 0.2
    // income * this
    val InitMoney = 100000
    // yen
    val SellLimitDay = 45
    val MakoInitialValue = 5000
    // yen
    val LandOpenedLimit = 10
    // if exceeds this, makomako land open
    val LandFeePerPerson = 1000 // yen
  }

  sealed trait Makomako {
    val id: String
    val day: Int
    def imgClass: String
    def esaKg: Double = {
      scala.math.min(day, Param.MaxmakoMaleWeight) * Param.EsaPerWeight
    }
    def cost: Double = esaKg * Param.ValuePerEsaWeight
  }

  case class OxMako(val id:String = randId, val day: Int = 0, daysInPG: Option[Int] = None) extends Makomako {
    def imgClass = day match {
      case d if d < 2 => "twa-hatching-chick"
      case d if d > Param.MaxMakoDay - 3 => "twa-skull twa-2x"
      case d if d > Param.SeimakoDay => "twa-ox twa-2x "
      case _ => "twa-ox"
    }

    def next() = {
      OxMako(id, day + 1, daysInPG.flatMap { d => if (d == 0) None else Some(d - 1) } orElse Some(15))
    }
  }

  case class DefaultMako(val id:String = randId, val day: Int = 0) extends Makomako {
    def imgClass = day match {
      case d if d < 2 => "twa-hatching-chick"
      case d if d > Param.MaxMakoDay - 3 => "twa-skull twa-2x"
      case d if d > Param.SeimakoDay => "twa-cow twa-2x "
      case _ => "twa-cow"
    }

    def next() = {
      DefaultMako(id, day + 1)
    }
  }

  case class State(secondsElapsed: Long, money: Int, diff:Int, makos: List[Makomako])

  type MakoClick = (String) => Unit

  def next(prev: State): State = {

    val newMakos = prev.makos collect {
      case ox: OxMako if ox.daysInPG == Some(0) =>
        val r = Random.nextInt(6)
        0.to(r).map{ _ => DefaultMako() }  ++ (if (Random.nextInt(8) == 0) List(OxMako()) else Nil)
    } flatten

    val donadona = prev.makos filter {
      case DefaultMako(_, d) => d == Param.SeimakoDay
      case _ => false
    }
    val donaMoney = donadona.length * 100

    val nextMakos = prev.makos.flatMap {
      case OxMako(_, d, _) if d==Param.MaxMakoDay => None
      case ox:OxMako => Some(ox.next)
      case DefaultMako(_, d) if d==Param.MaxMakoDay => None
      case m:DefaultMako => Some(m.next)
    }

    val cost = prev.makos.map{m => m.cost}.sum.toInt

    val diff =  - cost + donaMoney
    val next = State(prev.secondsElapsed + 1, prev.money + diff, diff, nextMakos ++ newMakos)
    console.log(next.toString)
    next
  }

  val makomako = ReactComponentB[(Makomako, MakoClick)]("makomako")
    .render(M => {
    val (m, c) = M
    div(cls := "picture", onClick --> c(m.id))(img(cls := "twa " + m.imgClass, title := "" /*s"${m.day} days"*/))
  }).build

  val makomakoList = ReactComponentB[(List[Makomako], MakoClick)]("makomakoList")
    .render(props => {
    val (ms, c) = props
    div(cls := "")(ms map { m => makomako((m, c)) })
  }).build

  class Backend($: BackendScope[_, State]) {
    var interval: js.UndefOr[js.timers.SetIntervalHandle] =
      js.undefined

    def onMakoClick(id: String):Unit = {
      val selled = $.state.makos.filter{ m=> m.id == id}.head

      if ( selled.isInstanceOf[DefaultMako] && selled.day > 30 ) {
        val deleted = $.state.makos.filter { m => m.id != id }
        val diff =  selled.day * Param.ValuePerMakoWeight
        $.modState(s => State(s.secondsElapsed, s.money+diff, diff, deleted))
      }
    }

    def tick() = {
      $.modState(s => next(s))
    }

    def start() =
      interval = js.timers.setInterval(1000)(tick())
  }

  val Timer = ReactComponentB[Unit]("Timer")
    .initialState(State(0, Param.InitMoney, 0, OxMako() :: Nil))
    .backend(new Backend(_))
    .render(props => {
    div( cls := "container",
      h4(props.state.secondsElapsed + " 日目 ")//props.state.money + " yen ", " ( " + props.state.diff +" )")
    (makomakoList((props.state.makos, props.backend.onMakoClick)))
    )
  })
    .componentDidMount(_.backend.start())
    .componentWillUnmount(_.backend.interval foreach js.timers.clearInterval)
    .buildU
}
