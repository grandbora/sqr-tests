package square

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {

  "only one word" >> {
    val dict = List("cat", "cats", "and", "sand", "dog")
    Solution.sentences("cat", dict) ==== List("cat")
    Solution.sentences("cats", dict) ==== List("cats")
    Solution.sentences("and", dict) ==== List("and")
    Solution.sentences("sand", dict) ==== List("sand")
  }

  "two word, one possibility" >> {
    val dict = List("cat", "cats", "and", "sand", "dog")
    Solution.sentences("catand", dict) ==== List("cat and")
    Solution.sentences("dogand", dict) ==== List("dog and")
    Solution.sentences("andcat", dict) ==== List("and cat")
  }

  "more than two words, one possibility" >> {
    val dict = List("cat", "cats", "and", "sand", "dog")
    Solution.sentences("catanddog", dict) ==== List("cat and dog")
    Solution.sentences("catanddogandcat", dict) ==== List("cat and dog and cat")
    Solution.sentences("dogcatandcat", dict) ==== List("dog cat and cat")
  }

  "two words, multiple possibilities" >> {
    val dict = List("cat", "cats", "and", "sand", "dog")
    Solution.sentences("catsand", dict) must haveSize(2)
    Solution.sentences("catsand", dict) must contain("cats and", "cat sand")
  }

  "complete example" >> {
    val dict = List("cat", "cats", "and", "sand", "dog")
    Solution.sentences("catsanddog", dict) must haveSize(2)
    Solution.sentences("catsanddog", dict) must contain("cats and dog", "cat sand dog")
  }
}
