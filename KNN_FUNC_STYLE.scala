import scala.util.Random
import scala.math

object KNN_FUNC_STYLE{
  //generate points
  def generatePoints(x:Float,y:Float,radius:Float,
                     pA:Array[Point],l:String):Array[Point]= {
    for(i<-0 to pA.length-1) {
      pA(i) = new Point()
      pA(i).x = x + rand.nextFloat() * radius
      pA(i).y=y+rand.nextFloat()*radius
      pA(i).l=l
    }
    return pA
  }
  //cal distance between p1 and p2
  val distance=(p1:Point,p2:Point)=>(
    math.sqrt(math.pow(p1.x-p2.x,2)+math.pow(p1.y-p2.y,2)).toFloat
    )

  def sort(dist:(Point,Point)=>Float,p: Point,pA:Array[Point]):Array[Point]= {
    return pA.drop(pA.indexOf(p)).sortWith(distance(p, _) < distance(p, _))
  }
  def topK(p:Point,pA:Array[Point],k:Int):Array[Point]={
    val pA1=pA.drop(pA.indexOf(p))
    val pA2=new Array[Point](k)
    for(i<-0 to k-1){
      //println('+')
      pA2(i)=pA1(i)
    }
    return pA2
  }

  val sortedDisIndicies = (p:Point,pA:Array[Point])=>pA.map { p1 =>
    distance(p,p1)
  }.zipWithIndex.sortBy(f => f._1).map(f => f._2)


  //class
  def classify(pA:Array[Point]): String ={
    import scala.collection.mutable.Map
    var counter=Map[String,Int]()
    var l=""
    for(p<-pA) {
      if(counter.contains(p.l))
        counter(p.l)+=1
      else
        counter(p.l)=1
    }
    var max=0
    for(k<-counter.keys)
      if(counter(k)>max){
        l=k
        max=counter(k)
      }
    return l
  }

  //print
  val showFunc=(pA:Array[Point])=>(pA.foreach(i=>println(i)))
  //some outer val
  val rand=new Random(17)
  var p1=new Array[Point](6)
  var p2=new Array[Point](7)




  def main(args: Array[String]): Unit = {
    p1=generatePoints(5,6,5,p1,"r")
    p2=generatePoints(7,9,6,p2,"b")
    showFunc(p1)
    showFunc(p2)
    var pA=Array.concat(p1,p2)
    //println(distance(p1(0),p2(0)))
    println("sort")
    showFunc(sort(distance,p1(0),pA))
    pA=sort(distance,p1(0),pA)
    println("top5")
    showFunc(topK(p2(0),pA,5))
    println()
    println(p2(0),"predicted label=",classify(topK(p2(0),pA,5)))

  }
}