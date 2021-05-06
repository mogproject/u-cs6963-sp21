// #pragma once
// #include "../io/parser.hpp"
// #include "../util.hpp"

// namespace mog {
// namespace cep {
// // int const N = 700;              // maximum number of vertices
// int const B = 64;               // bit length of a chunk
// // int const L = (N + B - 1) / B;  // number of chunks

// /**
//  * Vertex bitmap.
//  */
// template <int L>
// struct VMap {
//   static constexpr int const N = L * B;

//   unsigned long long data[L];

//   VMap(int n = 0) {
//     assert(0 <= n && n <= N);
//     memset(data, 0, sizeof(data));
//     for (int i = 0; i < L && n > 0; ++i, n -= B) { data[i] = (n >= B ? 0 : (1ULL << n)) - 1; }
//   }

//   inline bool empty() const {
//     for (int i = 0; i < L; ++i) {
//       if (data[i]) return false;
//     }
//     return true;
//   }

//   inline int front() const {
//     for (int i = 0; i < L; ++i)
//       if (data[i]) { return i * B + __builtin_ctzll(data[i]); }
//     return -1;
//   }

//   inline int pop_front() {
//     for (int i = 0; i < L; ++i)
//       if (data[i]) {
//         int x = i * B + __builtin_ctzll(data[i]);
//         this->operator^=(x);
//         return x;
//       }
//     return -1;
//   }

//   //--------------------------------------------------------
//   //    Operators
//   //--------------------------------------------------------

//   /**
//    * negation
//    */
//   VMap<L> operator~() {
//     return VMap<L>(N) - *this;
//   }

//   /**
//    * set
//    */
//   VMap<L>& operator|=(int x) {
//     assert(0 <= x && x < N);
//     data[x / B] |= 1ULL << (x % B);
//     return *this;
//   }

//   VMap<L>& operator|=(VMap<L> const& rhs) {
//     for (int i = 0; i < L; ++i) data[i] |= rhs.data[i];
//     return *this;
//   }

//   friend VMap<L> operator|(VMap<L> const& lhs, int x) {
//     assert(0 <= x && x < N);
//     VMap<L> ret(lhs);
//     ret |= x;
//     return std::move(ret);
//   }

//   friend VMap<L> operator|(VMap<L> const& lhs, VMap<L> const& rhs) {
//     VMap<L> ret;
//     for (int i = 0; i < L; ++i) ret.data[i] = lhs.data[i] | rhs.data[i];
//     return std::move(ret);
//   }

//   VMap<L>& operator^=(int x) {
//     assert(0 <= x && x < N);
//     data[x / B] ^= 1ULL << (x % B);
//     return *this;
//   }

//   VMap<L>& operator^=(VMap<L> const& rhs) {
//     for (int i = 0; i < L; ++i) data[i] ^= rhs.data[i];
//     return *this;
//   }

//   friend VMap<L> operator^(VMap<L> const& lhs, int x) {
//     assert(0 <= x && x < N);
//     VMap<L> ret(lhs);
//     ret ^= x;
//     return std::move(ret);
//   }

//   friend VMap<L> operator^(VMap<L> const& lhs, VMap<L> const& rhs) {
//     VMap<L> ret;
//     for (int i = 0; i < L; ++i) ret.data[i] = lhs.data[i] ^ rhs.data[i];
//     return std::move(ret);
//   }

//   VMap<L>& operator&=(int x) {
//     assert(0 <= x && x < N);
//     data[x / B] &= 1ULL << (x % B);
//     return *this;
//   }

//   VMap<L>& operator&=(VMap<L> const& rhs) {
//     for (int i = 0; i < L; ++i) data[i] &= rhs.data[i];
//     return *this;
//   }

//   friend VMap<L> operator&(VMap<L> const& lhs, int x) {
//     assert(0 <= x && x < N);
//     VMap<L> ret(lhs);
//     ret &= x;
//     return std::move(ret);
//   }

//   friend VMap<L> operator&(VMap<L> const& lhs, VMap<L> const& rhs) {
//     VMap<L> ret;
//     for (int i = 0; i < L; ++i) ret.data[i] = lhs.data[i] & rhs.data[i];
//     return std::move(ret);
//   }

//   /**
//    * reset
//    */
//   VMap<L>& operator-=(int x) {
//     assert(0 <= x && x < N);
//     data[x / B] &= ~(1ULL << (x % B));
//     return *this;
//   }

//   /**
//    * set minus
//    */
//   VMap<L>& operator-=(VMap<L> const& rhs) {
//     for (int i = 0; i < L; ++i) data[i] &= ~rhs.data[i];
//     return *this;
//   }

//   friend VMap<L> operator-(VMap<L> const& lhs, int x) {
//     assert(0 <= x && x < N);
//     VMap<L> ret(lhs);
//     ret -= x;
//     return std::move(ret);
//   }

//   friend VMap<L> operator-(VMap<L> const& lhs, VMap<L> const& rhs) {
//     VMap<L> ret;
//     for (int i = 0; i < L; ++i) ret.data[i] = lhs.data[i] & ~rhs.data[i];
//     return std::move(ret);
//   }

//   /**
//    * get
//    */
//   bool operator[](int x) const {
//     assert(0 <= x && x < N);
//     return (data[x / B] >> (x % B)) & 1ULL;
//   }

//   std::vector<int> to_vector() const {
//     std::vector<int> ret;
//     for (int i = 0; i < L; ++i)
//       if (data[i]) {
//         auto x = data[i];
//         while (x) {
//           ret.push_back(i * B + __builtin_ctzll(x));
//           x &= x - 1;
//         }
//       }
//     return std::move(ret);
//   }

//   std::size_t size() const {
//     int ret = 0;
//     for (int i = 0; i < L; ++i) ret += __builtin_popcountll(data[i]);
//     return ret;
//   }
// };

// /**
//  * equality
//  */
// template <int L>
// inline bool operator==(VMap<L> const& lhs, VMap<L> const& rhs) {
//   for (int i = 0; i < L; ++i) {
//     if (lhs.data[i] != rhs.data[i]) return false;
//   }
//   return true;
// }

// template <int L>
// inline bool operator!=(VMap<L> const& lhs, VMap<L> const& rhs) { return !(lhs == rhs); }

// template <int L>
// std::ostream& operator<<(std::ostream& os, VMap<L> const& vm) {
//   os << "VMap<" << L << ">({";
//   int i = 0;
//   for (auto x : vm.to_vector()) {
//     if (i++ > 0) os << ",";
//     os << x;
//   }
//   return os << "})";
// }

// }  // namespace cep
// }  // namespace mog