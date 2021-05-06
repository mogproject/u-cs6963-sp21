#include "../src/io/parser.hpp"
#include <fstream>
#include <gtest/gtest.h>

using namespace std;
using namespace mog::io;

TEST(ParserTest, DIMACSParserTest) {
  ifstream is("../../../../data/exact/exact001.gr");
  EXPECT_TRUE(is);

  auto el = DIMACSParser::parse(is);
  vector<pair<int, int>> edges = {
      {5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7},
  };

  EXPECT_EQ(el.n, 10);
  EXPECT_EQ(el.edges, edges);
}
