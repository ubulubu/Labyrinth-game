package game
import scala.collection.mutable.Buffer
import scala.util.Random

class LabGame {
  
  private var giveUp = false
  
  private def newMaze(floors: Int, height: Int, width: Int) = {
    val a = Buffer[Floor]()
    for(i <- 1 to floors){
      a += new Floor(height, width)
    }
    val maze = new Maze(a)
    maze.generate
    maze.floors.last.last.setTarget
    maze.floors.last.last.setGoal
    maze.floors(1).first.setUp(false)
    maze.floors(1).last.setUp(true)
    maze.floors(1).last.setTarget
    maze.floors.head.last.setTarget
    maze.floors.head.last.setUp(true)
    maze.floors.head.first.setPlayer(true)
    maze.floors.head.first.setStart
    maze
  }
  //adds stairs to the maze, a passable cell has a certain probability to be stairs.
  private def addStairs = {
    for(i <- 0 until maze.floors.head.height){
      for(j <- 0 until maze.floors.head.width){
      val rand = Random.nextInt(100)
      if(rand > 97 && this.maze.floors(0).board(i)(j).isPassable && this.maze.floors(1).board(i)(j).isPassable && !this.maze.floors(0).board(i)(j).isUp && !this.maze.floors(0).board(i)(j).isGoal){
        this.maze.floors(0).board(i)(j).setUp(true)
        this.maze.floors(0).board(i)(j).setTarget
        this.maze.floors(1).board(i)(j).setDown(true)
      }   
      }
  }
    for(i <- 0 until maze.floors.head.height){
      for(j <- 0 until maze.floors.head.width){
      val rand = Random.nextInt(100)
      if(rand > 97 && this.maze.floors(1).board(i)(j).isPassable && this.maze.floors(2).board(i)(j).isPassable && !this.maze.floors(1).board(i)(j).isUp && !this.maze.floors(0).board(i)(j).isGoal){
        this.maze.floors(1).board(i)(j).setUp(true)
        this.maze.floors(1).board(i)(j).setTarget
        this.maze.floors(2).board(i)(j).setDown(true)
  }
      }
    }
  }
  //Set up the maze with a player
  var maze = newMaze(3, 30, 50)
  addStairs
  val p = new Player(0, this.maze.floors(0).first.x, this.maze.floors(0).first.y)
  
  // win condition
  def win = {
    p.floor == (maze.floors.size - 1) && this.maze.floors(p.floor).board(p.x)(p.y).isGoal
  }
  
  def isGivenUp = this.giveUp
  
  //prints the whole maze
  private def printAll = {
    val txtMaze = maze.floors.map(_.toTxt)
    for(i <- txtMaze.indices){
    println((txtMaze(i).map(_.mkString).mkString("\n")) + "\n")
    }
    }
  
  //turns the current floor the player is on to a string
  def turnString = {
    if(!this.win){
      val txtmaze = maze.floors(p.floor).toTxt
      txtmaze.map(_.mkString).mkString("\n")
  }
    else if (isGivenUp) "Nice Try :)"
    else "YOU WIN!"
  }
  
  // turns the whole maze to a buffer[String]
  def toBufferString = {
    val txtMaze = maze.floors.map(_.toTxt)
    var a = Buffer[String]()
    for(i <- txtMaze.indices){
    a += (txtMaze(i).map(_.mkString).mkString("\n")) + "\n"
    }
    a
  }
  
  private def printThis = {
    val txtmaze = maze.floors(p.floor).toTxt
    print(txtmaze.map(_.mkString).mkString("\n"))
  }
  
  // set up file operations
  val fileOps = new FileOps
  
  //loads the game from the given file
  def loadGame(data: String){
    val a = fileOps.readFile(data)
    var b = Buffer[Floor]()
    for(i <- 1 to 3){
      b += new Floor(30, 50)
  }
    for (i <- 0 until 30){
      for(j <- 0 until 50){
        if (a(i)(j).toString == "G") {
          b(0).board(i)(j).setGoal
          b(0).board(i)(j).setTarget
        }
        else if (a(i)(j).toString == "P") {
          b(0).board(i)(j).setPlayer(true)
          b(0).board(i)(j).setPassable
          p.x = i
          p.y = j
          p.floor = 0
        }
        else if (a(i)(j).toString == "S") b(0).board(i)(j).setStart
        else if (a(i)(j).toString == "/") {
          b(0).board(i)(j).setUp(true)
          b(0).board(i)(j).setTarget
        }
        else if (a(i)(j).toString == raw"\") b(0).board(i)(j).setDown(true)
        else if (a(i)(j).toString == " " || a(i)(j).toString == ".") b(0).board(i)(j).setPassable
      }
    }
    for (i <- 31 until 61){
      for(j <- 0 until 50){
        if (a(i)(j).toString == "G") {
          b(1).board(i - 31)(j).setGoal
          b(1).board(i - 31)(j).setTarget
        }
        else if (a(i)(j).toString == "P") {
          b(1).board(i - 31)(j).setPlayer(true)
          b(1).board(i - 31)(j).setPassable
          p.x = i - 31
          p.y = j
          p.floor = 1
        }
        else if (a(i)(j).toString == "S") b(1).board(i - 31)(j).setStart
        else if (a(i)(j).toString == "/") {
          b(1).board(i - 31)(j).setUp(true)
          b(1).board(i - 31)(j).setTarget
        }
        else if (a(i)(j).toString == raw"\") b(1).board(i - 31)(j).setDown(true)
        else if (a(i)(j).toString == " " || a(i)(j).toString == ".") b(1).board(i - 31)(j).setPassable
  }
  }
    for (i <- 62 until 92){
      for(j <- 0 until 50){
        if (a(i)(j).toString == "G") {
          b(2).board(i - 62)(j).setGoal
          b(2).board(i - 62)(j).setTarget
          b(1).board(i - 62)(j).setTarget
        }
        else if (a(i)(j).toString == "P") {
          b(2).board(i - 62)(j).setPlayer(true)
          b(2).board(i - 62)(j).setPassable
          p.x = i - 62
          p.y = j
          p.floor = 2
        }
        else if (a(i)(j).toString == "S") b(2).board(i - 62)(j).setStart
        else if (a(i)(j).toString == "/") {
          b(2).board(i - 62)(j).setUp(true)
          b(1).board(i - 62)(j).setTarget
        }
        else if (a(i)(j).toString == raw"\") b(2).board(i - 62)(j).setDown(true)
        else if (a(i)(j).toString == " " || a(i)(j).toString == ".") b(2).board(i - 62)(j).setPassable
    }
    }
    maze = new Maze(b)
  }
  
  // moves the player towards the given direction
  def movePlayer(dir: Char) = {
    dir match{
    case 'w' => {
      if(maze.floors(p.floor).board(p.x - 1)(p.y).isPassable) {
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.x = p.x - 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
        
    }
    case 's' => {
      if(maze.floors(p.floor).board(p.x + 1)(p.y).isPassable) {
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.x = p.x + 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
    }
    case 'a' => {
      if(maze.floors(p.floor).board(p.x)(p.y - 1).isPassable){
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.y = p.y - 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
    }
    case 'd' => {
      
      if(maze.floors(p.floor).board(p.x)(p.y + 1).isPassable) {
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.y = p.y + 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
    }
    case 'e' => {
      if(maze.floors(p.floor).board(p.x)(p.y).isPassable && maze.floors(p.floor).board(p.x)(p.y).isUp) {
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.floor = p.floor + 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
      else if (maze.floors(p.floor).board(p.x)(p.y).isPassable && maze.floors(p.floor).board(p.x)(p.y).isDown){
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(false)
        p.floor = p.floor - 1
        maze.floors(p.floor).board(p.x)(p.y).setPlayer(true)
        
      }
    }
    case _ =>
  }
  }
  
  //shows the solution to the maze
  def solveThis = {
    giveUp = true  
    
    val a = maze.floors(p.floor).solver(p.x, p.y)
    val a2 = maze.floors(1).solver(maze.floors(1).solver(a.x, a.y).x, maze.floors(1).solver(a.x, a.y).y)
    val a3 = maze.floors(2).solver(maze.floors(2).solver(a2.x, a2.y).x, maze.floors(2).solver(a2.x, a2.y).y)
      
  }
}