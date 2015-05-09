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
  }

  sealed trait Manju {
    val id: String
    val day: Int
    def imgClass: String
  }

  case class DefaultManju(val id:String = randId, val day: Int = 0) extends Manju {
    def imgClass = day match {
      case d if d < 2 => "twa-hatching-chick"
      case _ => "twa-cow twa-2x "
    }

    def next() = {
      DefaultManju(id, day + 1)
    }
  }

  case class State(secondsElapsed: Long, money: Int, diff:Int, manjus: List[Manju], params: List[Parameter])

  case class Parameter(name: String, v: Int)

  type UpDownClick = (String) => Unit

  val inputParameter = ReactComponentB[(Parameter, UpDownClick, UpDownClick)]("param")
  .render(props => {
    val (p, upClick, downClick) = props
    div(
    h3(p.name + ":" + p.v),
      div(onClick --> upClick(p.name))(img(cls := "twa twa-allow-up", alt := "↑")),
      div(onClick --> downClick(p.name))(img(cls := "twa twa-allow-down", alt := "↓"))
    )
  }).build

  val inputParameterList = ReactComponentB[(List[Parameter], UpDownClick, UpDownClick)]("paramList")
    .render(props => {
    val (ps, up, down) = props
    div(cls := "")(ps map { m => inputParameter((m, up, down)) })
  }).build


  type MakoClick = (String) => Unit

  def next(prev: State): State = {

    val nextMakos = prev.manjus.flatMap {
      case m:DefaultManju => Some(m.next)
    }

    val next = State(prev.secondsElapsed + 1, prev.money, 0, nextMakos, prev.params)
    console.log(next.toString)
    next
  }

  val manju = ReactComponentB[(Manju, MakoClick)]("manju")
    .render(M => {
    val (m, c) = M
    div(cls := "picture", onClick --> c(m.id))(img(cls := "twa " + m.imgClass, title := "" /*s"${m.day} days"*/))
  }).build

  val manjuList = ReactComponentB[(List[Manju], MakoClick)]("manjuList")
    .render(props => {
    val (ms, c) = props
    div(cls := "")(ms map { m => manju((m, c)) })
  }).build

  class Backend($: BackendScope[_, State]) {
    var interval: js.UndefOr[js.timers.SetIntervalHandle] =
      js.undefined

    def onMakoClick(id: String):Unit = {
      val selled = $.state.manjus.filter{ m=> m.id == id}.head

      if ( selled.isInstanceOf[DefaultManju] && selled.day > 30 ) {
        val deleted = $.state.manjus.filter { m => m.id != id }
        $.modState(s => State(s.secondsElapsed, s.money+0, 0, deleted, s.params))
      }
    }

    def upOrDown(isUp:Boolean)(n:String):Unit = {

      val newOne = $.state.params.map { i =>
        if (i.name == n) {
          val diff = if (isUp) {
            10
          } else {
            -10
          }
          Parameter(i.name, i.v + diff)
        } else {
          i
        }
      }

      $.modState(s => State(s.secondsElapsed, s.money, s.diff, s.manjus, newOne))
    }

    def tick() = {
      //$.modState(s => next(s))
    }

    def start() =
      interval = js.timers.setInterval(1000)(tick())
  }

  val Timer = ReactComponentB[Unit]("Timer")
    .initialState(State(0, 0, 0, Nil, List(Parameter("a", 0), Parameter("b", 0), Parameter("c", 0))))
    .backend(new Backend(_))
    .render(props => {
    div( cls := "container",
      h4(props.state.secondsElapsed + " 日目 ")//props.state.money + " yen ", " ( " + props.state.diff +" )")
        (inputParameterList((props.state.params, props.backend.upOrDown(true), props.backend.upOrDown(false))))
//    (makomakoList((props.state.makos, props.backend.onMakoClick)))

    )
  })
    .componentDidMount(_.backend.start())
    .componentWillUnmount(_.backend.interval foreach js.timers.clearInterval)
    .buildU
}
