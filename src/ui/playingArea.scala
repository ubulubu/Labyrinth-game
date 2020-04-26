package ui
import scala.swing._
import scala.swing.event._
import javax.swing.UIManager
import game._

object playingArea extends SimpleSwingApplication{
  
  var newGame = new labGame
  
  val startButton = new Button("Start Game")
  val solveButton = new Button("Solve")
  val loadButton = new Button("Load")
  val saveButton = new Button("Save")
  val newGameButton= new Button("New Game")
  

  val outputArea = new TextArea(newGame.turnString, 31, 51)
  outputArea.font = new Font("Consolas", 1, 22)
  
  outputArea.editable = false
  outputArea.lineWrap = true

  // Layout
  val botRow = new FlowPanel
  botRow.contents += solveButton
  botRow.contents += loadButton
  botRow.contents += saveButton
  botRow.contents += newGameButton

  val wholeLayout = new BoxPanel(Orientation.Vertical)
  wholeLayout.contents += outputArea
  wholeLayout.contents += botRow
  

  val window = new MainFrame
  window.title = "Labyrinth Game"
  window.resizable = true
  window.contents = wholeLayout
  
  // Reactions
  this.listenTo(newGameButton)
  this.listenTo(loadButton)
  this.listenTo(saveButton)
  this.listenTo(solveButton)
  this.listenTo(outputArea.keys)
  this.reactions += {
    case key: KeyTyped => if(!newGame.win){
      newGame.movePlayer(key.char)
      outputArea.text = newGame.turnString
    }
    case event: ButtonClicked => {
      if(event.source == solveButton){
        newGame.solveThis
        outputArea.text = newGame.turnString
        Dialog.showMessage(wholeLayout, "Solved!" + "\n" + "If this is a loaded maze, only a path to the next floor is showed instead", "Notification")
      }
      else if (event.source == saveButton){
        val s = Dialog.showInput(wholeLayout,"Name the file, remember to put .txt at the end","Notification",Dialog.Message.Plain, Swing.EmptyIcon,Nil, "")
        if(s.isDefined) newGame.fileOps.writeToFile(s.get, newGame.toBufferString)
      }
      else if (event.source == loadButton){
        val s = Dialog.showInput(wholeLayout,"Load from: ","Notification",Dialog.Message.Plain, Swing.EmptyIcon,Nil, "")
        if(s.isDefined) newGame.loadGame(s.get)
        outputArea.text = newGame.turnString
      }
      else if (event.source == newGameButton){
        newGame = new labGame
        outputArea.text = newGame.turnString
      }
    }
  }
  def top = this.window
  
}