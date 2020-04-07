package game

class cell (val x:Int, val y:Int){
  
  var isPassable = false
  var isStaircase = false
  var isGoal = false
  var isVisited = false
  var isStart = false
  var isSolution = false
  
  def setPassable = {
    this.isPassable = true
  }

  def setStairs = {
    this.isStaircase = true
  }
  
  def setGoal = {
    this.isGoal = true
  }
  def setWall = {
    this.isPassable = false
  }
  def setVisited = {
    this.isVisited = true
  }
  def setStart = {
    this.isStart = true
  }
  def setSolution = {
    this.isSolution = true
  }
}