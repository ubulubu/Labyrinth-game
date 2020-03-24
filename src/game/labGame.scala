package game

object labGame extends App{
  val a = new maze(50, 50, 1)
  a.Prim
  val b = a.toTxt
  
  print(b.map(_.mkString).mkString("\n"))
}