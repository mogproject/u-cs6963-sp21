#include "../src/cep/branch.hpp"
#include "../src/io/parser.hpp"
#include "../src/util.hpp"
#include <fstream>
#include <gtest/gtest.h>

using namespace std;
using namespace mog::io;
using namespace mog::cep;
using namespace mog::data;

Reducer r;
LowerBounder lb;
UpperBounder ub;

template <int L>
void verify_result(Graph<L> g, std::vector<Edge> const& edits, int k) {
  for (auto& e : edits) g.edit_edge(e.first, e.second);
  if (!g.is_cluster_graph() || edits.size() != k) { print(edits); }

  EXPECT_TRUE(g.is_cluster_graph());
  EXPECT_EQ(edits.size(), k);
}

void test_with_el(EdgeListInput const& el, int expected) {
  static constexpr int const L = 1;
  auto g = Graph<L>(el);
  BrancherNaive<L> br;
  auto bnb = BranchAndBound<L>(&r, &lb, &ub, (Brancher<L>*)&br);
  auto result = bnb.run(g);

  if (expected == 0) {
    EXPECT_TRUE(g.is_cluster_graph());
  } else {
    EXPECT_FALSE(g.is_cluster_graph());
  }

  verify_result(g, result, expected);
}

void test_provided(int id, int expected) {
  char buf[100];
  sprintf(buf, "../../../../data/exact/exact%03d.gr", id);
  ifstream is(buf);
  EXPECT_TRUE(is);

  test_with_el(DIMACSParser::parse(is), expected);
}

TEST(BranchTest, SmallGraphs) {
  test_with_el({3, {{0, 1}, {0, 2}}}, 1);
  test_with_el({4, {{0, 1}, {0, 2}, {1,2},{1,3}}}, 1);
  test_with_el({4, {{0, 1}, {0, 2}, {2,3}}}, 1);
  test_with_el({5, {{1, 2}, {1, 3}, {1, 4}, {1, 0}, {2, 3}, {2, 4}, {2, 0}, {3, 4}, {3, 0}, {4, 0}}}, 0);  // K_5
  test_with_el({5, {{0, 1}, {0, 2}, {0, 3}, {1, 4}, {2, 4}, {3, 4}}}, 4);
  test_with_el({5, {{0, 1}, {1, 2}, {1, 4}, {0, 3}, {3, 4}}}, 3);                                  // banner
  test_with_el({5, {{0, 1}, {1, 2}, {2, 3}, {3, 4}, {4, 0}}}, 3);                                  // C_5
  test_with_el({5, {{0, 1}, {1, 2}, {2, 0}, {1, 3}, {2, 4}}}, 2);                                  // bull
  test_with_el({5, {{0, 1}, {1, 2}, {2, 3}, {3, 0}, {0, 4}, {1, 4}, {2, 4}, {3, 4}}}, 2);          // W_4
  test_with_el({5, {{1, 2}, {1, 3}, {1, 4}, {1, 0}, {2, 3}, {2, 4}, {3, 4}, {3, 0}, {4, 0}}}, 1);  // K_5 - e

  test_with_el({10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7}}}, 3);

  test_provided(1, 3);
  // test_provided(3, 42);
}
