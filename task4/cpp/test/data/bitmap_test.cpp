#include "../../src/data/bitmap.hpp"
#include <gtest/gtest.h>

using namespace std;
using namespace mog::data;

TEST(BitmapTest, BitmapTest) {
  Bitmap<700> b;
  EXPECT_EQ(b.to_vector(), std::vector<int>({}));
  b |= 0;
  EXPECT_EQ(b.to_vector(), std::vector<int>({0}));
  b |= 1;
  b |= 3;
  b |= 200;
  b |= 590;
  b |= 699;
  EXPECT_EQ(b.to_vector(), std::vector<int>({0, 1, 3, 200, 590, 699}));
  b ^= 590;
  b ^= 3;
  EXPECT_EQ(b.to_vector(), std::vector<int>({0, 1, 200, 699}));
  EXPECT_TRUE(b[0]);
  EXPECT_TRUE(b[1]);
  EXPECT_TRUE(b[200]);
  EXPECT_TRUE(b[699]);
  EXPECT_FALSE(b[698]);

  Bitmap<700> b2;
  b2 |= 1;
  b2 |= 3;
  b2 |= 200;
  b2 |= 201;
  EXPECT_EQ((b & b2).size(), 2);

  // construct with 1-bits
  Bitmap<127> b3({0, 1, 2, 5, 125}), b4({5, 125, 126});
  EXPECT_EQ(b3, Bitmap<127>({125, 5, 2, 2, 2, 1, 0}));
  EXPECT_EQ(b4, Bitmap<127>({125, 5, 126, 5}));

  EXPECT_EQ(b3 | 63, Bitmap<127>({0, 1, 2, 5, 125, 63}));
  EXPECT_EQ(b3 | 125, Bitmap<127>({0, 1, 2, 5, 125}));
  EXPECT_EQ(b3 & 63, Bitmap<127>());
  EXPECT_EQ(b3 & 125, Bitmap<127>(125));
  EXPECT_EQ(b3 ^ 63, Bitmap<127>({0, 1, 2, 5, 125, 63}));
  EXPECT_EQ(b3 ^ 125, Bitmap<127>({0, 1, 2, 5}));
  EXPECT_EQ(b3 - 63, Bitmap<127>({0, 1, 2, 5, 125}));
  EXPECT_EQ(b3 - 125, Bitmap<127>({0, 1, 2, 5}));

  EXPECT_EQ(b3 | b4, Bitmap<127>({0, 1, 2, 5, 125, 126}));
  EXPECT_EQ(b3 & b4, Bitmap<127>({5, 125}));
  EXPECT_EQ(b3 ^ b4, Bitmap<127>({0, 1, 2, 126}));
  EXPECT_EQ(b3 - b4, Bitmap<127>({0, 1, 2}));

  EXPECT_TRUE((b3).subset(b3));
  EXPECT_TRUE((b3 - b4).subset(b3));
  EXPECT_FALSE((b3).subset(b3 - b4));

  EXPECT_TRUE((b3).superset(b3));
  EXPECT_FALSE((b3 - b4).superset(b3));
  EXPECT_TRUE((b3).superset(b3 - b4));

  Bitmap<0> bm0;
  Bitmap<1> bm1;
  bm1 = ~bm1;
  Bitmap<2> bm2;
  bm2 = ~bm2;
  Bitmap<16> bm16;
  bm16 = ~bm16;
  Bitmap<32> bm32;
  bm32 = ~bm32;
  Bitmap<63> bm63;
  bm63 = ~bm63;
  Bitmap<64> bm64;
  bm64 = ~bm64;

  EXPECT_EQ(bm0.size(), 0);
  EXPECT_EQ(bm1.size(), 1);
  EXPECT_EQ(bm2.size(), 2);
  EXPECT_EQ(bm16.size(), 16);
  EXPECT_EQ(bm32.size(), 32);
  EXPECT_EQ(bm63.size(), 63);
  EXPECT_EQ(bm64.size(), 64);

  Bitmap<128> bb({50, 90, 127});
  EXPECT_EQ(bb.front(), 50);
  EXPECT_EQ(bb.pop_front(), 50);
  EXPECT_EQ(bb.front(), 90);
  EXPECT_EQ(bb.pop_front(), 90);
  EXPECT_EQ(bb.front(), 127);
  EXPECT_EQ(bb.pop_front(), 127);
  EXPECT_EQ(bb.front(), -1);
  EXPECT_EQ(bb.pop_front(), -1);
}

TEST(BitmapTest, EncodeTest) {
  Bitmap<128> b0, b1(0);
  EXPECT_EQ(b0.encode(), "00000000000000000000000000000000");
  EXPECT_EQ(b1.encode(), "00000000000000000000000000000001");
  EXPECT_EQ((~b0).encode(), "ffffffffffffffffffffffffffffffff");
  EXPECT_EQ((~b1).encode(), "fffffffffffffffffffffffffffffffe");
}