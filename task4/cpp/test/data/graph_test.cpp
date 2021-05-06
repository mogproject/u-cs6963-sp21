#include "../../src/data/graph.hpp"
#include <gtest/gtest.h>

using namespace std;
using namespace mog::data;

#define mp make_pair

TEST(GraphTest, GraphComponentTest) {
  mog::io::EdgeListInput el = {10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7}}};
  Graph<1> g(el);

  auto cc = g.components();

  EXPECT_EQ(cc.size(), 4);
  EXPECT_EQ(cc[0].to_vector(), std::vector<int>({0}));
  EXPECT_EQ(cc[1].to_vector(), std::vector<int>({3}));
  EXPECT_EQ(cc[2].to_vector(), std::vector<int>({1, 2}));
  EXPECT_EQ(cc[3].to_vector(), std::vector<int>({4, 5, 6, 7, 8, 9}));
}

TEST(GraphTest, GraphDegreeTest) {
  mog::io::EdgeListInput el = {10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7}}};
  Graph<1> g(el);

  EXPECT_EQ(g.degree(0), 0);
  EXPECT_EQ(g.degree(1), 1);
  EXPECT_EQ(g.degree(2), 1);
  EXPECT_EQ(g.degree(4), 3);
  EXPECT_EQ(g.degree(7), 5);
}

TEST(GraphTest, GraphPropertyTest) {
  Graph<1> g({5, {{1, 2}, {1, 3}, {1, 4}, {1, 0}, {2, 3}, {2, 4}, {2, 0}, {3, 4}, {3, 0}, {4, 0}}});
  EXPECT_TRUE(g.is_cluster_graph());
}

TEST(GraphTest, GraphFindP3Test) {
  mog::io::EdgeListInput el = {10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7}}};
  Graph<1> g(el);

  EXPECT_EQ(g.find_p3(), make_tuple(8, 7, 4));

  Graph<1> g2({5, {{0, 1}, {1, 2}, {1, 4}, {0, 3}, {3, 4}}});
  g2.lock(0, 1);
  EXPECT_EQ(g2.find_p3(), make_tuple(2, 1, 0));
}

TEST(GraphTest, GraphEditEdgeTest) {
  mog::io::EdgeListInput el = {3, {{0, 1}, {0, 2}}};
  Graph<1> g(el);
  Matrix<64, false> vse = vector<Edge>({{1, 1}, {1, 2}, {2, 0}});
  Matrix<64, true> n1e = vector<Edge>({mp(0, 1), mp(0, 2)});
  Matrix<64, true> n2e = vector<Edge>({mp(1, 2)});
  Matrix<64, true> pme = vector<Edge>({{0, 1}, {0, 2}, {1, 2}});
  EXPECT_EQ(g.nbr1, n1e);
  EXPECT_EQ(g.nbr2, n2e);
  EXPECT_EQ(g.perm, pme);

  g.edit_edge(0, 1);
  Matrix<64, false> vsea = vector<Edge>({{0, 1}, {1, 0}, {1, 2}});
  Matrix<64, true> n1ea = vector<Edge>({{0, 2}});
  Matrix<64, true> n2ea = vector<Edge>({});
  Matrix<64, true> pmea = vector<Edge>({{0, 2}});
  EXPECT_EQ(g.vertices, vsea);
  EXPECT_EQ(g.nbr1, n1ea);
  EXPECT_EQ(g.nbr2, n2ea);
  EXPECT_EQ(g.perm, pmea);

  EXPECT_TRUE(g.nbr2[0].empty());
  EXPECT_TRUE(g.nbr2[1].empty());
  EXPECT_TRUE(g.nbr2[2].empty());

  Graph<1> g2(el);
  g2.edit_edge(1, 0);
  EXPECT_EQ(g2.vertices, vsea);
  EXPECT_EQ(g2.nbr1, n1ea);
  EXPECT_EQ(g2.nbr2, n2ea);
  EXPECT_EQ(g2.perm, pmea);

  EXPECT_TRUE(g2.nbr2[0].empty());
  EXPECT_TRUE(g2.nbr2[1].empty());
  EXPECT_TRUE(g2.nbr2[2].empty());

  Graph<1> g3(el);
  g3.edit_edge(2, 1);
  EXPECT_EQ(g3.vertices, vector<Edge>({{2, 0}, {2, 1}, {2, 2}}));
  EXPECT_EQ(g3.nbr1, vector<Edge>({{0, 1}, {0, 2}, {1, 2}}));
  EXPECT_EQ(g3.nbr2, vector<Edge>());
  EXPECT_EQ(g3.perm, vector<Edge>({{0, 1}, {0, 2}}));
}

// TEST(GraphTest, GraphNumTrianglesTest) {
//   mog::io::EdgeListInput el = {10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7},
//   {4, 7}}}; Graph g(el);

//   EXPECT_EQ(g.num_triangles, 6);

//   Graph g2(&g, 5, 8);
//   EXPECT_EQ(g2.num_triangles, 8);

//   Graph g3(&g, 9, 5);
//   EXPECT_EQ(g3.num_triangles, 5);

//   Graph g4(&g, {{5, 9}, {7, 9}, {7, 8}});
//   EXPECT_EQ(g4.num_triangles, 4);
// }

TEST(GraphTest, GraphPermTest) {
  mog::io::EdgeListInput el = {10, {{5, 6}, {5, 7}, {4, 5}, {4, 6}, {5, 9}, {7, 9}, {7, 8}, {8, 9}, {1, 2}, {6, 7}, {4, 7}}};
  Graph<1> g(el);

  for (int i = 0; i < 10; ++i) cout << g.perm[i] << endl;
  EXPECT_FALSE(g.perm[1][4]);
  EXPECT_TRUE(g.perm[4][7]);
  EXPECT_TRUE(g.perm[5][8]);
  EXPECT_TRUE(g.perm[8][5]);

  g.edit_edge(5, 8);
  EXPECT_FALSE(g.perm[1][4]);
  EXPECT_FALSE(g.perm[5][8]);
  EXPECT_FALSE(g.perm[8][5]);
}

TEST(GraphTest, GraphHideVertexTest) {
  mog::io::EdgeListInput el = {3, {{0, 1}, {0, 2}}};
  Graph<1> g(el);

  EXPECT_EQ(g.get_vertices(), std::vector<int>({1, 2, 0}));
  g.hide_vertex(1);
  EXPECT_EQ(g.get_vertices(), std::vector<int>({2, 0}));
  g.hide_vertex(0);
  EXPECT_EQ(g.get_vertices(), std::vector<int>({2}));
}