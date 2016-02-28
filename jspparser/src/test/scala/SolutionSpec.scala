package square

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {

  "only html tag" >> {
    val input =
      "<!DOCTYPE html>" +
        "<html>" +
        "</html>"

    val expected =
      """System.out.print("<!DOCTYPE html><html></html>");"""

    Solution.render(input) ==== expected
  }

  "only jsp evaluation tag" >> {

    val input =
      "<% int i = 3 % 5, j = 2; %>"

    val expected =
      "int i = 3 % 5, j = 2; ;"

    Solution.render(input) ==== expected
  }

  "only jsp render tag" >> {

    val input =
      "<%= System.currentTimeMillis() %>"

    val expected =
      "System.out.print( System.currentTimeMillis() );"

    Solution.render(input) ==== expected
  }

  "jsp eval tag occurs only once in html" >> {

    "html first jsp evaluation last" >> {

      val inputHtml =
        "<!DOCTYPE html>" +
          "<html>" +
          "</html>"

      val inputJsp =
        "<% int i = 3 % 5, j = 2; %>"

      val expected =
        """System.out.print("<!DOCTYPE html><html></html>");""" +
          "int i = 3 % 5, j = 2; ;"

      Solution.render(inputHtml + inputJsp) ==== expected

    }

    "jsp evaluation first html last" >> {

      val inputHtml =
        "<!DOCTYPE html>" +
          "<html>" +
          "</html>"

      val inputJsp =
        "<% int i = 3 % 5, j = 2; %>"

      val expected =
        "int i = 3 % 5, j = 2; ;" +
          """System.out.print("<!DOCTYPE html><html></html>");"""

      Solution.render(inputJsp + inputHtml) ==== expected
    }

    "jsp evaluation in the middle" >> {

      val inputHtmlBeginning =
        "<!DOCTYPE html>" +
          "<html>"

      val inputJsp =
        "<% int i = 3 % 5, j = 2; %>"

      val inputHtmlEnding =
        "</html>"

      val expected =
        """System.out.print("<!DOCTYPE html><html>");""" +
          "int i = 3 % 5, j = 2; ;" +
          """System.out.print("</html>");"""


      Solution.render(inputHtmlBeginning + inputJsp + inputHtmlEnding) ==== expected
    }
  }

  "jsp render tag occurs only once in html" >> {

    "html first jsp evaluation last" >> {

      val inputHtml =
        "<!DOCTYPE html>" +
          "<html>" +
          "</html>"

      val inputJsp =
        "<%= System.currentTimeMillis() %>"

      val expected =
        """System.out.print("<!DOCTYPE html><html></html>");""" +
          "System.out.print( System.currentTimeMillis() );"

      Solution.render(inputHtml + inputJsp) ==== expected

    }

    "jsp render tag in the middle" >> {
      val input =
        "<!DOCTYPE html>" +
          "<html>" +
          "<h1>Time: <%= System.currentTimeMillis() %></h1>" +
          "</html>"


      val expected =
        """System.out.print("<!DOCTYPE html><html><h1>Time: ");""" +
          "System.out.print( System.currentTimeMillis() );" +
          """System.out.print("</h1></html>");"""

      Solution.render(input) ==== expected
    }
  }

  "complete example" >> {

    val input = "" +
      "<!DOCTYPE html>" +
      "<html>" +
      "  <head>" +
      "    <% int i = 3 % 5, j = 2; %>" +
      "    <title>JSP Parser!</title>" +
      "  </head>" +
      "  <body>" +
      "    <h1>Time: <%= System.currentTimeMillis() %></h1>" +
      "    <p><code>i = <%=i%></code> and that's 33% more than <code>j = <%=j%></p>" +
      "  </body>" +
      "</html>"

    val expected =
      """System.out.print("<!DOCTYPE html><html>  <head>    ");""" +
        """int i = 3 % 5, j = 2; ;""" +
        """System.out.print("    <title>JSP Parser!</title>  </head>  <body>    <h1>Time: ");""" +
        """System.out.print( System.currentTimeMillis() );""" +
        """System.out.print("</h1>    <p><code>i = ");""" +
        """System.out.print(i);""" +
        """System.out.print("</code> and that's 33% more than <code>j = ");""" +
        """System.out.print(j);""" +
        """System.out.print("</p>  </body></html>");"""

    Solution.render(input) ==== expected

  }
}


























