#include "../../src/data/matrix.hpp"
#include <gtest/gtest.h>

#define p make_pair

using namespace std;
using namespace mog::data;

TEST(MatrixTest, MatrixTest) {
  typedef pair<int, int> P;
  typedef vector<P> V;

  typedef Matrix<567, false> M567F;
  typedef Matrix<567, true> M567T;
  EXPECT_EQ(M567F::ident().size(), 567);
  EXPECT_EQ(M567T::ident().size(), 567);

  Matrix<700, true> m1;
  EXPECT_EQ(m1.to_vector(), V({}));
  m1 |= p(200, 300);
  EXPECT_EQ(m1.to_vector(), V({{200, 300}}));
  EXPECT_TRUE(m1[200][300]);
  EXPECT_FALSE(m1[200][200]);
  EXPECT_FALSE(m1[300][300]);

  m1 |= p(400, 400);
  EXPECT_EQ(m1.to_vector(), V({{200, 300}, {400, 400}}));
  m1 |= p(300, 200);
  EXPECT_EQ(m1.to_vector(), V({{200, 300}, {400, 400}}));
  m1 ^= p(7, 0);
  EXPECT_EQ(m1.to_vector(), V({{0, 7}, {200, 300}, {400, 400}}));
  m1 ^= p(3, 4);
  EXPECT_EQ(m1.to_vector(), V({{0, 7}, {3, 4}, {200, 300}, {400, 400}}));
  m1 ^= p(3, 4);
  EXPECT_EQ(m1.to_vector(), V({{0, 7}, {200, 300}, {400, 400}}));
  m1 -= p(0, 7);
  EXPECT_EQ(m1.to_vector(), V({{200, 300}, {400, 400}}));
  m1 &= p(400, 400);
  EXPECT_EQ(m1.to_vector(), V({{400, 400}}));
  m1 &= p(300, 400);
  EXPECT_EQ(m1.to_vector(), V({}));

  Matrix<30, false> m2({{3, 3}, {1, 5}, {2, 9}, {4, 10}, {29, 28}, {29, 29}, {28, 29}});
  EXPECT_EQ(m2.capacity(), 30);
  EXPECT_EQ(m2.size(), 7);
  EXPECT_EQ((~m2).size(), 900 - 7);
  EXPECT_EQ(m2.front(), P({1, 5}));
  EXPECT_EQ(m2.pop_front(), P({1, 5}));
  EXPECT_EQ(m2.front(), P({2, 9}));
  EXPECT_EQ(m2.pop_front(), P({2, 9}));
  EXPECT_EQ(m2.front(), P({3, 3}));
  EXPECT_EQ(m2.pop_front(), P({3, 3}));
  EXPECT_EQ(m2.front(), P({4, 10}));
  EXPECT_EQ(m2.pop_front(), P({4, 10}));
  EXPECT_EQ(m2.front(), P({28, 29}));
  EXPECT_EQ(m2.pop_front(), P({28, 29}));
  EXPECT_EQ(m2.front(), P({29, 28}));
  EXPECT_EQ(m2.pop_front(), P({29, 28}));
  EXPECT_EQ(m2.front(), P({29, 29}));
  EXPECT_EQ(m2.pop_front(), P({29, 29}));
  EXPECT_EQ(m2.front(), P({-1, -1}));
  EXPECT_EQ(m2.pop_front(), P({-1, -1}));

  Matrix<30, true> m3({{3, 3}, {1, 5}, {2, 9}, {4, 10}, {29, 28}, {29, 29}, {28, 29}});
  EXPECT_EQ(m3.capacity(), 30);
  EXPECT_EQ(m3.size(), 6);
  EXPECT_EQ((~m3).size(), 30 * 31 / 2 - 6);
  EXPECT_EQ(m3.front(), P({1, 5}));
  EXPECT_EQ(m3.pop_front(), P({1, 5}));
  EXPECT_EQ(m3.front(), P({2, 9}));
  EXPECT_EQ(m3.pop_front(), P({2, 9}));
  EXPECT_EQ(m3.front(), P({3, 3}));
  EXPECT_EQ(m3.pop_front(), P({3, 3}));
  EXPECT_EQ(m3.front(), P({4, 10}));
  EXPECT_EQ(m3.pop_front(), P({4, 10}));
  EXPECT_EQ(m3.front(), P({28, 29}));
  EXPECT_EQ(m3.pop_front(), P({28, 29}));
  EXPECT_EQ(m3.front(), P({29, 29}));
  EXPECT_EQ(m3.pop_front(), P({29, 29}));
  EXPECT_EQ(m3.front(), P({-1, -1}));
  EXPECT_EQ(m3.pop_front(), P({-1, -1}));

  Matrix<3, true> m4({{0, 1}, {0, 2}});
  Matrix<3, true> m5({{0, 2}});
  EXPECT_EQ(m4 - P({1, 0}), m5);
  EXPECT_EQ(m4 ^ P({1, 0}), m5);
  m4 ^= P({1, 0});
  EXPECT_EQ(m4, m5);
  EXPECT_EQ(m4 ^ P({0, 1}), m4 ^ P({1, 0}));
  EXPECT_EQ(m4 - P({0, 1}), m4 - P({1, 0}));
}