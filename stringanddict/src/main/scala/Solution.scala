package square


//s = "catsanddog",
//dict = ["cat", "cats", "and", "sand", "dog"].
//
//A solution is ["cats and dog", "cat sand dog"].

object Solution extends App {
  def sentences(s: String, dict: List[String]): List[String] = {

    if (dict.contains(s))
      List(s)
    else {
      dict.filter(s.startsWith(_)) match {
        case Nil => ???
        case beginnings =>
          beginnings.map{
            beginning =>
              sentences(s.toCharArray.drop(beginning .length).mkString(""), dict).map {
                rest =>
                  beginning  + " " + rest
              }
          }.flatten
      }
    }
  }
}
