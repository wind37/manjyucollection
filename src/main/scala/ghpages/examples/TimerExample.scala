package ghpages.examples

import japgolly.scalajs.react._, vdom.all._
import scala.scalajs.js
import ghpages.examples.util.SideBySide
import org.scalajs.dom.console

import scala.util.Random

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
    def imgSrc: String
    def secondsToComplete: Int
  }

  case class DefaultManju(val id:String = randId, val day: Int = 0) extends Manju {
    def imgClass = day match {
      case d if d < 2 => "twa-hatching-chick"
      case _ => "twa-cow twa-2x "
    }

    def imgSrc = "http://ecx.images-amazon.com/images/I/41IIf83jj5L.jpg"

    def next() = {
      DefaultManju(id, day + 1)
    }

    def secondsToComplete = 30
  }

  case class State(secondsElapsed: Long, money: Int, diff:Int, manjus: List[Manju], resources: List[Resource], inputResources: List[Resource], makers: List[Maker])

  case class Resource(name: String, v: Int)

  type UpDownClick = (String) => Unit

  val inputParameter = ReactComponentB[(Resource, UpDownClick, UpDownClick)]("param")
    .render(props => {
    val (p, upClick, downClick) = props
    div(cls := "column",
      div(cls := "ui vertical divider"),
      div(cls := "ui two column middle aligned centered grid",
          div(cls := "center aligned column", div(cls := "ui huge right aligned header", p.name + ":" + p.v)),
          div(cls := "column",
            div(cls := "ui vertical buttons",
              div(cls := "ui icon button", onClick --> upClick(p.name), (i(cls := "arrow circle up icon", title := "↑")), "+10"),
              div(cls := "ui icon button", onClick --> downClick(p.name), (i(cls := "arrow circle down icon", title := "↓")), "-10")
            ))))
  }).build

  val inputParameterList = ReactComponentB[(List[Resource], UpDownClick, UpDownClick)]("paramList")
    .render(props => {
    val (ps, up, down) = props
    div(cls:="ui segment",
      div(cls := "ui two column centered grid")(ps map { m => inputParameter((m, up, down)) })
    )
  }).build

  case class Maker(id:String, isEmpty:Boolean, restSeconds: Int, becomed: Manju)

  val maker = ReactComponentB[(Maker, MakoClick, MakoClick)]("maker")
    .render(props => {
    val (m, onMake, onOpen) = props
    div(cls:="column",
      (div(cls := "ui vertical divider")),
      if (m.isEmpty) {
        div(cls := "ui button", onClick --> onMake(m.id), "作成!")
      } else if (m.restSeconds == 0) {
        div(cls := "ui button", onClick --> onOpen(m.id), "完成!")
      } else {
        val hour = m.restSeconds / 3600
        val min = (m.restSeconds-(hour*3600)) / 60
        val sec = (m.restSeconds - (hour*3600) - min * 60)
        div("%02d:%02d:%02d".format(hour, min, sec))
      }
    )
  }).build

  val makerList = ReactComponentB[(List[Maker], MakoClick, MakoClick)]("makoList")
    .render(props => {
    val (ps, make, open) = props
    div(cls := "ui segment",
      div(cls := "ui three column middle aligned centered grid")(ps map { m => maker((m, make, open)) })
    )
  }).build

  type MakoClick = (String) => Unit

  def next(prev: State): State = {

    // 資材が貯まる

    // 工房のカウントダウン
    val newMakers = prev.makers.map {m =>
      if (m.isEmpty) {
        m
      } else {
        val sec = if (m.restSeconds == 0) 0 else m.restSeconds - 1
        Maker(m.id, m.isEmpty, sec, m.becomed)
      }
    }

    val next = State(prev.secondsElapsed + 1, prev.money, 0, prev.manjus, prev.resources, prev.inputResources, newMakers)
    //console.log(next.toString)
    next
  }

  def makeItFrom(params: List[Resource]):(Int, Manju) = {
    val m = DefaultManju()
    (m.secondsToComplete, m)
  }

  val manju = ReactComponentB[(Manju, MakoClick)]("manju")
    .render(M => {
    val (m, c) = M
    div(cls := "card",
      div(cls := "image", onClick --> c(m.id))(img(cls:="ui small image", src := m.imgSrc)),
      div(cls := "content small", "")
    )
  }).build

  val manjuList = ReactComponentB[(List[Manju], MakoClick)]("manjuList")
    .render(props => {
    val (ms, c) = props
    div(cls:="ui segment",
    div(cls := "ui three doubling cards")(ms map { m => manju((m, c)) })
    )
  }).build

  class Backend($: BackendScope[_, State]) {
    var interval: js.UndefOr[js.timers.SetIntervalHandle] =
      js.undefined

    def onMakoClick(id: String):Unit = {
      val selled = $.state.manjus.filter{ m=> m.id == id}.head

      if ( selled.isInstanceOf[DefaultManju] && selled.day > 30 ) {
        val deleted = $.state.manjus.filter { m => m.id != id }
        $.modState(s => State(s.secondsElapsed, s.money+0, 0, deleted, s.resources, s.inputResources, s.makers))
      }
    }

    def upOrDown(isUp:Boolean)(n:String):Unit = {

      val newOne = $.state.inputResources.map { i =>
        if (i.name == n) {
          val diff = if (isUp) {
            10
          } else {
            -10
          }
          Resource(i.name, i.v + diff)
        } else {
          i
        }
      }

      $.modState(s => State(s.secondsElapsed, s.money, s.diff, s.manjus, s.resources, newOne, s.makers))
    }

    def doMakeIt(id: String):Unit = {
      val newOne = $.state.makers.map { i =>
        def okRes():Boolean = {
          $.state.inputResources.forall(r => r.v >= $.state.resources.find(_.name == r.name).get.v)
        }
        if (i.id == id) {
          val (sec, m) = makeItFrom($.state.inputResources)
          Maker(i.id, false, sec, m)
        } else {
          i
        }
      }

      val newRes = $.state.resources.map { r =>
        val usedRes = $.state.inputResources.find(_.name == r.name).get
        if ( r.v >= usedRes.v) {
          Resource(r.name, r.v - usedRes.v)
        } else {
          r
        }
      }

      $.modState(s => State(s.secondsElapsed, s.money, s.diff, s.manjus, newRes, s.inputResources, newOne))
    }

    def doOpenIt(id: String):Unit = {
      val newOne = $.state.makers.find(_.id == id).get.becomed

      val reseted = $.state.makers.map { i =>
        if (i.id == id) {
          Maker(i.id, true, 0, null)
        } else {
          i
        }
      }

      $.modState(s => State(s.secondsElapsed, s.money, s.diff, s.manjus ++ List(newOne), s.resources, s.inputResources, reseted))
    }

    def tick() = {
      $.modState(s => next(s))
    }

    def start() =
      interval = js.timers.setInterval(1000)(tick())
  }

  def initialState():State = {
    val ress = List(Resource("米粉", 0), Resource("水", 0), Resource("砂糖", 0), Resource("小豆", 0))
    State(0, 0, 0, List(), ress.map(i=>Resource(i.name, 1000)), ress, List(Maker("0", true, 0, null)))
  }

  val Timer = ReactComponentB[Unit]("Timer")
    .initialState(initialState())
    .backend(new Backend(_))
    .render(props => {
      div(cls := "ui segment",
        div(cls:= "ui large left aligned header", "Lv.0 " + "あなたの名前"),
        div(cls := "ui large right aligned header",
          props.state.resources.map(r => r.name + ":" + r.v).mkString(" ")
        )
          (div(cls := "ui divider", "工房"))
          (inputParameterList((props.state.inputResources, props.backend.upOrDown(true), props.backend.upOrDown(false))))
        (makerList((props.state.makers, props.backend.doMakeIt, props.backend.doOpenIt)))
        (div(cls := "ui divider", "手持ちのまんじゅう"))
        (manjuList((props.state.manjus, props.backend.onMakoClick)))
    )
  })
    .componentDidMount(_.backend.start())
    .componentWillUnmount(_.backend.interval foreach js.timers.clearInterval)
    .buildU
}
