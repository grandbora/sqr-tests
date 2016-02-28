package square

/**
  * JSP Tags:
  *
  * <% xyz %>
  *
  * where "xyz" is pure Java code
  *
  * <%= xyz %>
  *
  * where "xyz" is passed to System.out.println.
  *
  * System.out.print("<!DOCTYPE html><html>  <head>    ");
  * int i = 3 % 5, j = 2; ;
  * System.out.print("    <title>JSP Parser!</title>  </head>  <body>    <h1>Time: ");
  * System.out.print( System.currentTimeMillis() );
  * System.out.print("</h1>    <p><code>i = ");
  * System.out.print(i);
  * System.out.print("</code> and that's 33% more than <code>j = ");
  * System.out.print(j);
  * System.out.print("</p>  </body></html>");
  */
//public static String parse(String jsp) {
//// TODO Implement me!
//return "";
//}

//public static void main(String... args) {
//String jsp = ""
//+ "<!DOCTYPE html>"
//+ "<html>"
//+ "  <head>"
//+ "    <% int i = 3 % 5, j = 2; %>"
//+ "    <title>JSP Parser!</title>"
//+ "  </head>"
//+ "  <body>"
//+ "    <h1>Time: <%= System.currentTimeMillis() %></h1>"
//+ "    <p><code>i = <%=i%></code> and that's 33% more than <code>j = <%=j%></p>"
//+ "  </body>"
//+ "</html>";


//System.out.println(parse(jsp));
//}

//}

object Solution extends App {

  def render(input: String): String = {
    input.toCharArray.toList match {
      case Nil => ""
      case '<' :: '%' :: '=' :: tail => renderJsp(tail, renderJspRender)
      case '<' :: '%' :: tail => renderJsp(tail, renderJspEval)
      case _ => renderHtml(input)
    }
  }

  private def renderJspEval(jsp: List[Char]):String = (jsp.drop(1) :+ ';').mkString("")
  private def renderJspRender(jsp: List[Char]):String = s"""System.out.print(${jsp.mkString("")});"""

  private def renderJsp(jspTail: List[Char], renderProcess: List[Char] => String): String = {

    val jspEnding = jspTail.mkString("").indexOf("%>")

    if (jspEnding == -1)
      throw new Exception("unclosed jsp tag")
    else {

      val jspPart = jspTail.take(jspEnding)
      val htmlPart = jspTail.drop(jspEnding + 2).mkString("")

      renderProcess(jspPart) + render(htmlPart)
    }
  }

  private def renderHtml(html: String): String = {

    val jspBeginning = html.indexOf("<%")

    if (jspBeginning == -1)
      s"""System.out.print("$html");"""
    else {
      val htmlPart = html.toCharArray.toList.take(jspBeginning).mkString("")
      val jspPart = html.toCharArray.toList.drop(jspBeginning).mkString("")
      s"""System.out.print("$htmlPart");""" + render(jspPart)
    }
  }
}



















