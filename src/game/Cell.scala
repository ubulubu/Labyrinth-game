package game

class Cell (val x:Int, val y:Int){
  // states of the cell
  var isPassable = false
  var isUp = false
  var isDown = false
  var isGoal = false
  var isVisited = false
  var isStart = false
  var isSolution = false
  var isTarget = false
  var containsP = false
  
  // methods to alter the states
  def setPassable = {
    this.isPassable = true
  }

  def setUp (a: Boolean)= {
    this.isUp = a
    this.setPassable
  }
  
  def setDown (a: Boolean)= {
    this.isDown = a
    this.setPassable
  }
  
  def setGoal = {
    this.isGoal = true
    this.setPassable
  }
  def setWall = {
    this.isPassable = false
  }
  def setVisited = {
    this.isVisited = true
  }
  def setStart = {
    this.isStart = true
    this.setPassable
  }
  def setSolution = {
    this.isSolution = true
  }
  def setTarget = {
    this.isTarget = true
  }
  def setPlayer(a: Boolean) = {
    containsP = a
  }
}