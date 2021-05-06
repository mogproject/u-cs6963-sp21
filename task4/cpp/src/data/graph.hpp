#pragma once
#include "../io/parser.hpp"
#include "matrix.hpp"

#define ASSERT_RANGE_2D(x, y) assert(0 <= (x) && (x) < N && 0 <= (y) && (y) < N)

namespace mog {
namespace data {

typedef std::pair<int, int> Edge;

/**
 * Graph representation.
 */
template <int L>
class Graph {
 public:
  static constexpr int const N = L * Bitmap<0>::B;
  typedef Bitmap<N> BM;

  int n;
  int num_edited;
  Matrix<N, false> vertices;  // row: degree, col: vertex IDs
  Matrix<N, true> nbr1;       // adjacency matrix (symmetric)
  Matrix<N, true> nbr2;       // radius-2 neighbors (symmetric)
  Matrix<N, true> perm;       // editable vertex pairs

  // identity matrix
  Matrix<N, true> ident() { return Matrix<N, true>::ident(); }

 public:
  Graph() {}

  /**
   * Construct a graph from input.
   *
   * Complexity: O(n^2 L)
   */
  Graph(mog::io::EdgeListInput const& el) : num_edited(0), n(el.n) {
    // edges
    for (auto& e : el.edges) nbr1 |= e;
    // vertices (compute degrees)
    for (int v = 0; v < el.n; ++v) vertices |= std::make_pair(degree(v), v);
    // r2 neighborhood
    nbr2 = closed_neighborhood_r2() - nbr1 - ident();
    // permission
    perm = nbr1 | nbr2;
  }

  /**
   * Complexity: O(nL)
   */
  size_t num_vertices() const { return vertices.size(); }

  /**
   * Complexity: O(nL)
   */
  size_t num_edges() const { return nbr1.size(); }

  /**
   * Complexity: O(L)
   */
  int degree(int v) const { return nbr1[v].size(); }

  /**
   * Complexity: O(1)
   */
  inline void lock(int v, int u) { perm -= std::make_pair(v, u); }

  /**
   * Complexity: O(nL)
   */
  std::vector<int> get_vertices() const {
    std::vector<int> ret;
    for (auto& p : vertices.to_vector()) ret.push_back(p.second);
    return std::move(ret);
  }

  /**
   * Complexity:
   */
  std::vector<BM> components() const {
    std::vector<BM> ret;
    BM visited;

    for (auto v : get_vertices()) {
      if (visited[v]) continue;

      // BFS
      BM c = BM(v), q(v);
      visited |= v;

      while (true) {
        auto u = q.pop_front();
        if (u == -1) break;

        auto frontier = nbr1[u] - visited;
        if (frontier.empty()) continue;

        q |= frontier;
        c |= frontier;
        visited |= frontier;
      }
      ret.push_back(c);
    }
    return ret;
  }

  bool is_clique(BM const& vs) const {
    int cnt = 0;
    for (auto v : vs.to_vector()) cnt += degree(v);
    return cnt == vs.size() * (vs.size() - 1);
  }

  bool is_cluster_graph() const {
    auto cc = components();
    return std::all_of(cc.begin(), cc.end(), [this](BM const& c) { return this->is_clique(c); });
  }

  /**
   * Complexity:
   */
  void remove_vertex(int v) {
    auto cm = Matrix<N, true>::cross_matrix(v);
  
    Matrix<N, false> vtx_diff;
    Matrix<N, true> nbr2_diff;
    vtx_diff -= std::make_pair(degree(v), v);
    nbr2_diff = cm;
  
    for (auto u: nbr1[v].to_vector()) {
      auto degu = degree(u);
      vtx_diff ^= std::make_pair(degu, u);
      vtx_diff ^= std::make_pair(degu - 1, u);
      for (auto w: (nbr1[v] & nbr2[u]).to_vector()) {
        if (u < w && ((nbr1[u]) & (nbr1[w]) - v).empty()) {
          // u-v-w is the only u-w path of distance 2
          nbr2_diff |= std::make_pair(u, w);
        }
      }
    }

    vertices -= vtx_diff;
    nbr1 -= cm;
    nbr2 -= nbr2_diff;
  }

  /**
   * Complexity: O(L)
   */
  void hide_vertex(int v) {
    // assert(vertices[degree(v)][v]);
    vertices[degree(v)] -= v;
  }

  /**
   * Complexity: (nL)
   */
  void hide_vertices(BM const& vs) {
    for (int i = 0; i < N; ++i) vertices[i] -= vs;
  }

  /**
   * Complexity:
   */
  void edit_edge(int v, int u) {
    assert(v != u);
    ++num_edited;
    int degv = degree(v);
    int degu = degree(u);
    nbr1 ^= std::make_pair(v, u);
    perm -= std::make_pair(v, u);

    assert(vertices[degv][v]);
    assert(vertices[degu][u]);
    vertices[degv] -= v;
    vertices[degu] -= u;

    if (!nbr1[v][u]) {
      // edge deletion
      vertices[degv - 1] |= v;  // decrement v's degree
      vertices[degu - 1] |= u;  // decrement u's degree
      
      if (!(nbr1[v] & nbr1[u]).empty()) nbr2 -= std::make_pair(v, u);  // d_G(v,u)=1 -> d_G'(v,u)>2

      // when w-v-u or w-u-v is the only distance-2 path
      for (auto w : (nbr1[v] & nbr2[u]).to_vector()) {
        if ((nbr1[w] & nbr1[u]).empty()) {
          nbr2 -= std::make_pair(w, u);
          perm -= std::make_pair(w, u);
        }
      }
      for (auto w : (nbr2[v] & nbr1[u]).to_vector()) {
        if ((nbr1[v] & nbr1[w]).empty()) {
          nbr2 -= std::make_pair(v, w);
          perm -= std::make_pair(v, w);
        }
      }
    } else {
      // edge addition
      vertices[degv + 1] |= v;  // increment v's degree
      vertices[degu + 1] |= u;  // increment u's degree

      nbr2 -= std::make_pair(v, u);

      // if u and w are not adjacent, then w-u becomes distance 2
      for (auto w : (nbr1[v] - nbr1[u] - u).to_vector()) nbr2 |= std::make_pair(w, u);
      for (auto w : (nbr1[u] - nbr1[v] - v).to_vector()) nbr2 |= std::make_pair(v, w);
    }
  }

  /**
   * Complexity: O(nL)
   */
  // TODO: compact into one int
  std::tuple<int, int, int> find_p3() const {
    for (auto v : get_vertices()) {
      auto u = nbr2[v].front();
      if (u >= 0) {
        // std::cout << v << " " << u << " " << nbr1[v] << " " << nbr1[u] << std::endl;
        auto w = (nbr1[v] & nbr1[u]).front();
        return std::make_tuple(v, w, u);
      }
    }
    return std::make_tuple(-1, -1, -1);
  }

  /**
   * Find a P_3 including the given vertex.
   */
  std::tuple<int, int, int> find_p3(int v) const {
    if (nbr2[v].empty()) {
      // auto ws = nbr1[v] & nbr2[u];
      for (auto u: nbr1[v].to_vector()) {
        auto w = (nbr1[v] & nbr2[u]).front();
        if (w >= 0) {
          return std::make_tuple(u, v, w);
        }
      }
    } else {
      auto u = nbr2[v].front();
      auto w = (nbr1[v] & nbr1[u]).front();
      return std::make_tuple(v, w, u);
    }
    return std::make_tuple(-1, -1, -1);
  }


 private:
  /**
   * Complexity: O(n^2 L)
   */
  Matrix<N, true> closed_neighborhood_r2() const {
    Matrix<N, true> ret;
    for (auto v : get_vertices()) {
      for (auto u : nbr1[v].to_vector()) {
        //
        ret[v] |= nbr1[u];
      }
    }
    assert(ret.is_symmetric());
    return std::move(ret);
  }

};  // namespace data

}  // namespace data
}  // namespace mog