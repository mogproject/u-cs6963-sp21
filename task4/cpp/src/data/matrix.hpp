#pragma once
#include "bitmap.hpp"

#define ASSERT_RANGE_2D(x, y) assert(0 <= (x) && (x) < N && 0 <= (y) && (y) < N)

namespace mog {
namespace data {

/**
 * Square matrix.
 */
template <int N, bool Symmetric>
class Matrix {
  typedef Matrix<N, Symmetric> T;

  Bitmap<N> data_[N];

 public:
  static constexpr T ident() {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] |= i;
    return std::move(ret);
  }

  static constexpr T cross_matrix(int x) {
    assert(0 <= x && x < N);
    T ret;
    Bitmap<N> epty, sngl;
    sngl |= x;
    for (int i = 0; i < N; ++i) ret.data_[i] = i == x ? (~epty) : sngl;
    return std::move(ret);
  }

  // each row has the same values
  static constexpr T clique_matrix(Bitmap<N> const& xs) {
    T ret;
    for (auto x: xs.to_vector()) ret.data_[x] = xs - x;
    return std::move(ret);
  }

  Matrix() {}
  /**
   * Constructs a singleton.
   */
  Matrix(int x, int y) { *this |= std::make_pair(x, y); }
  /**
   * Constructs from a list.
   */
  Matrix(std::vector<std::pair<int, int>> xys) {
    for (auto& xy : xys) {
      ASSERT_RANGE_2D(xy.first, xy.second);
      data_[xy.first] |= xy.second;
      if (Symmetric) data_[xy.second] |= xy.first;
    }
  }

  inline bool empty() const {
    for (int i = 0; i < N; ++i) {
      if (!data_[i].empty()) return false;
    }
    return true;
  }

  inline std::pair<int, int> front() const {
    for (int i = 0; i < N; ++i) {
      auto j = data_[i].front();
      if (j >= 0) return {i, j};
    }
    return {-1, -1};
  }

  inline std::pair<int, int> pop_front() {
    for (int i = 0; i < N; ++i) {
      auto j = data_[i].pop_front();
      if (j >= 0) {
        if (Symmetric) data_[j] -= i;
        return {i, j};
      }
    }
    return {-1, -1};
  }

  bool is_symmetric() const {
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        if (i != j && data_[i][j] != data_[j][i]) return false;
      }
    }
    return true;
  }

  //--------------------------------------------------------
  //    Operators
  //--------------------------------------------------------

  /**
   * negation (not)
   */
  T operator~() const {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] = ~data_[i];
    return std::move(ret);
  }

  /**
   * set/union (or)
   */
  T& operator|=(std::pair<int, int> const& xy) {
    ASSERT_RANGE_2D(xy.first, xy.second);
    data_[xy.first] |= xy.second;
    if (Symmetric && xy.first != xy.second) data_[xy.second] |= xy.first;
    return *this;
  }

  T& operator|=(T const& rhs) {
    for (int i = 0; i < N; ++i) data_[i] |= rhs.data_[i];
    return *this;
  }

  friend T operator|(T const& lhs, std::pair<int, int> const& xy) {
    T ret(lhs);
    ret |= xy;
    return std::move(ret);
  }

  friend T operator|(T const& lhs, T const& rhs) {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] = lhs.data_[i] | rhs.data_[i];
    return std::move(ret);
  }

  /**
   * exclusive or (xor)
   */
  T& operator^=(std::pair<int, int> const& xy) {
    ASSERT_RANGE_2D(xy.first, xy.second);
    data_[xy.first] ^= xy.second;
    if (Symmetric && xy.first != xy.second) data_[xy.second] ^= xy.first;
    return *this;
  }

  T& operator^=(T const& rhs) {
    for (int i = 0; i < N; ++i) data_[i] ^= rhs.data_[i];
    return *this;
  }

  friend T operator^(T const& lhs, std::pair<int, int> const& xy) {
    T ret(lhs);
    ret ^= xy;
    return std::move(ret);
  }

  friend T operator^(T const& lhs, T const& rhs) {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] = lhs.data_[i] ^ rhs.data_[i];
    return std::move(ret);
  }

  /**
   * intersection (and)
   */
  T& operator&=(std::pair<int, int> const& xy) { return *this &= T(xy.first, xy.second); }

  T& operator&=(T const& rhs) {
    for (int i = 0; i < N; ++i) data_[i] &= rhs.data_[i];
    return *this;
  }

  friend T operator&(T const& lhs, std::pair<int, int> const& xy) {
    T ret(lhs);
    ret &= xy;
    return std::move(ret);
  }

  friend T operator&(T const& lhs, T const& rhs) {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] = lhs.data_[i] & rhs.data_[i];
    return std::move(ret);
  }

  /**
   * reset/set minus
   */
  T& operator-=(std::pair<int, int> xy) {
    ASSERT_RANGE_2D(xy.first, xy.second);
    data_[xy.first] -= xy.second;
    if (Symmetric && xy.first != xy.second) data_[xy.second] -= xy.first;
    return *this;
  }

  T& operator-=(T const& rhs) {
    for (int i = 0; i < N; ++i) data_[i] -= rhs.data_[i];
    return *this;
  }

  friend T operator-(T const& lhs, std::pair<int, int> xy) {
    T ret(lhs);
    ret -= xy;
    return std::move(ret);
  }

  friend T operator-(T const& lhs, T const& rhs) {
    T ret;
    for (int i = 0; i < N; ++i) ret.data_[i] = lhs.data_[i] - rhs.data_[i];
    return std::move(ret);
  }

  /**
   * get
   */
  Bitmap<N> const& operator[](int x) const {
    assert(0 <= x && x < N);
    return data_[x];
  }

  // non-const version
  Bitmap<N>& operator[](int x) {
    assert(0 <= x && x < N);
    return data_[x];
  }

  std::vector<std::pair<int, int>> to_vector() const {
    std::vector<std::pair<int, int>> ret;
    for (int i = 0; i < N; ++i) {
      for (auto j : data_[i].to_vector()) {
        if (!Symmetric || i <= j) { ret.push_back(std::make_pair(i, j)); }
      }
    }
    return std::move(ret);
  }

  /**
   * equality
   */
  friend inline bool operator==(T const& lhs, T const& rhs) {
    for (int i = 0; i < N; ++i) {
      if (lhs.data_[i] != rhs.data_[i]) return false;
    }
    return true;
  }

  friend inline bool operator!=(T const& lhs, T const& rhs) { return !(lhs == rhs); }

  int capacity() const { return N; }

  std::size_t size() const {
    std::size_t ret = 0;
    for (int i = 0; i < N; ++i) ret += data_[i].size();

    if (Symmetric) {
      // double-count I
      for (int i = 0; i < N; ++i) {
        if (data_[i][i]) ++ret;
      }

      // halve the value
      ret /= 2;
    }
    return ret;
  }

  friend std::ostream& operator<<(std::ostream& os, Matrix<N, Symmetric> const& m) {
    os << "Matrix<" << N << "," << Symmetric << ">({" << std::endl;
    for (int i = 0; i < N; ++i) {
      os << m.data_[i];
      if (i < N - 1) os << ",";
      os << std::endl;
    }
    return os << "})";
  }
};

}  // namespace data
}  // namespace mog