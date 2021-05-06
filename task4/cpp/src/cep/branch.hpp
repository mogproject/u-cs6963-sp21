#pragma once
#include "../data/graph.hpp"
#include <unordered_set>
#include <vector>

namespace mog {
namespace cep {

static constexpr int const B = mog::data::Bitmap<0>::B;
static constexpr int const INF = 2000000000;

class Reducer {
 public:
  /**
   * Complexity: O(n^2 L)
   */
  template <int L>
  void reduce_and_lock(mog::data::Graph<L> &g) const {
    //------------------------------------------------------
    // 1. Bridge decomposition
    //------------------------------------------------------
    // Run BFS and find bridges.
    // Remove all bridges adjacent to a non-bridge.
    // Find maximum matching among the briges and keep them.
    //------------------------------------------------------
    // TODO: implement

    //------------------------------------------------------
    // 2. Two permanent non-edges
    //------------------------------------------------------
    // O(n^2 L)
    if (true) {
      for (auto v : g.get_vertices()) {
        for (auto u : g.perm[v].to_vector()) {
          // v-u is editable
          auto x = g.nbr1[v] & (~g.perm[v]);     // locked edges incident to v
          auto y = (~g.nbr1[u]) & (~g.perm[u]);  // locked non-edges incident to u

          auto z = x & y - v - u;
          if (!z.empty()) {
            // std::cout << "[REDUCTION] " << v << "-" << u << ":" << z << std::endl;
            if (g.nbr1[v][u]) {
              // remove edge vu
              g.edit_edge(v, u);
            } else {
              // lock v-u
              g.lock(v, u);
            }
          }
        }
      }
    }

    //------------------------------------------------------
    // 3. Smallgraph resolution
    //------------------------------------------------------
    // Find small components.
    // Check for isomorphism.
    // Look up tables and modify edges accordingly.
    //------------------------------------------------------

    // TODO: implement

    //------------------------------------------------------
    // 4. Bipartite graph resolution
    //------------------------------------------------------
    // TODO: implement

    //------------------------------------------------------
    // 5. True twin lock
    //------------------------------------------------------
    // mog::data::Edges between true twins (u,v s.t. N[u]=N[v]) cannot be editable.
    //------------------------------------------------------
    for (auto v : g.get_vertices()) {
      for (auto u : g.nbr1[v].to_vector()) {
        if (v < u && (g.nbr1[v] | v) == (g.nbr1[u] | u)) {
          // TRACE("[TWIN LOCK] %d-%d\n", v, u);
          // std::cout << (g.nbr1[v] | v) << std::endl;
          // std::cout << (g.nbr1[u] | u) << std::endl;
          // std::cout << ((g.nbr1[v] | v) == (g.nbr1[u] | u) ? "T" : "F") << std::endl;
          g.lock(v, u);
        }
      }
    }

    //------------------------------------------------------
    // 6. Two permanent edges
    //------------------------------------------------------
    // TODO: implement

    //------------------------------------------------------
    // 7. Update the vertex set to consider
    //------------------------------------------------------
    for (auto v : g.get_vertices()) {
      if (g.perm[v].empty()) g.hide_vertex(v);
    }
  }

 private:
};

class LowerBounder {
 public:
  template <int L>
  int lower_bound(mog::data::Graph<L> g) const {
    // P3 packing
    int ret = 0;

    int n = g.num_vertices();
    while (n >= 3) {
      auto v_ = g.vertices.front().second;  // choose a vertex with the min-degree
      auto p3 = g.find_p3(v_);
      int v = std::get<0>(p3);
      if (v < 0) {
        // no P_3 including v => N[v] forms a clique
        auto vv = g.nbr1[v_] | v_;
        g.hide_vertices(vv);
        n -= vv.size();
        continue;
      }

      g.remove_vertex(v);
      g.remove_vertex(std::get<1>(p3));
      g.remove_vertex(std::get<2>(p3));
      ++ret;
      n -= 3;
    }

    return ret;
  }
};

class UpperBounder {
 public:
  template <int L>
  int upper_bound(mog::data::Graph<L> const &g, int lower_bound, mog::data::Matrix<L * B, true> &cert) const {
    static constexpr int const N = L * B;
    auto components = g.components();
    auto tmp = g.nbr1;

    int cnt = 0;
    for (auto &c : components) {
      // number of vertices in the component
      int n = c.size();

      // number of edges in the component
      int m = 0;
      for (auto v : c.to_vector()) { m += g.nbr1[v].size(); }
      int missing = n * (n - 1) / 2 - m / 2;

      // component-wise greedy matching
      auto matched = pack_p2(g, c);
      // std::cout << "matched: " << matched << std::endl;
      auto cm = mog::data::Matrix<N, true>::clique_matrix(c);

      if (m - matched.first < missing) {
        // remove all edges in this component other than the matched ones
        tmp -= cm;
        tmp |= matched.second;
      } else {
        // make clique
        // add all edges in this component
        tmp |= cm;
      }
      cnt += std::min(m - matched.first, missing);
    }

    // copy certificate
    if (lower_bound == cnt) {
      cert = tmp;
    }
    return cnt;
  }

 private:
  // copy g
  template <int L>
  std::pair<int, mog::data::Matrix<L * B, true>> pack_p2(mog::data::Graph<L> const &g_,
                                                         typename mog::data::Graph<L>::BM const &target) const {
    mog::data::Graph<L> g(g_);
    mog::data::Matrix<L * B, true> matched;

    for (int i = 0; i < L * B; ++i) g.vertices[i] &= target;  // filter vertices

    int ret = 0;

    int n = g.num_vertices();
    while (n >= 2) {
      auto v = g.vertices.front().second;
      auto u = g.nbr1[v].front();
      if (u < 0) {
        // v is isolated
        n -= 1;
        g.remove_vertex(v);
      } else {
        ++ret;
        n -= 2;
        g.remove_vertex(v);
        g.remove_vertex(u);
        matched |= std::make_pair(v, u);
      }
    }
    return std::make_pair(ret, matched);
  }
};

template <int L>
class Brancher {
 public:
  virtual std::vector<mog::data::Edge> branch(mog::data::Graph<L> const &g) const = 0;
};

template <int L>
class BrancherNaive : Brancher<L> {
 public:
  std::vector<mog::data::Edge> branch(mog::data::Graph<L> const &g) const {
    auto p3 = g.find_p3();
    int v = std::get<0>(p3);
    int u = std::get<1>(p3);
    int w = std::get<2>(p3);

    TRACE("P3: %d %d %d\n", v, u, w);

    if (v == -1) return {};
    assert(u != -1);
    assert(w != -1);

    std::vector<mog::data::Edge> ret;
    if (g.perm[v][u]) ret.push_back({v, u});
    if (g.perm[v][w]) ret.push_back({v, w});
    if (g.perm[u][w]) ret.push_back({u, w});
    return std::move(ret);
  }
};

template <int L>
class BranchAndBound {
 public:
  typedef mog::data::Graph<L> G;
  static constexpr int const N = L * B;

  Reducer *reducer;
  LowerBounder *lower_bounder;
  UpperBounder *upper_bounder;
  Brancher<L> *brancher;
  int best;
  mog::data::Matrix<N, true> cert;

  BranchAndBound(Reducer *reducer, LowerBounder *lower_bounder, UpperBounder *upper_bounder, Brancher<L> *brancher)
      : reducer(reducer), lower_bounder(lower_bounder), upper_bounder(upper_bounder), brancher(brancher) {}

  std::vector<mog::data::Edge> run(G const& g) {
    best = INF;
    TRACE("NEW INSTANCE%s\n", "");
    search(g);

    // collect result
    return (g.nbr1 ^ cert).to_vector();
  }

  // int count_edge_editing(mog::data::Graph const *g) const {
  //   int ret = 0;
  //   for (auto v: root->get_vertices()) {
  //     ret += (root->nbr1[v] ^ g->nbr1[v]).size();
  //   }
  //   return ret / 2;
  // }

  void dump_graph(mog::data::Graph<L> const &g, int n = 5) const {
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        printf("%c%c%c", g.perm[i][j] ? ' ' : '[', g.nbr1[i][j] ? '1' : '0', g.perm[i][j] ? ' ' : ']');
      }
      printf("\n");
    }
    printf("----\n");
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        printf("%c%c%c", g.perm[i][j] ? ' ' : '[', g.nbr2[i][j] ? '1' : '0', g.perm[i][j] ? ' ' : ']');
      }
      printf("\n");
    }
  }

  void search(mog::data::Graph<L> const &parent) {
    TRACE("[search] start: k=%d\n", parent.num_edited);
    // dump_graph(parent);

    // 1. Reduce
    TRACE("  [reduce] %s\n", "start");
    G g(parent);
    reducer->reduce_and_lock(g);
    TRACE("  [reduce] end   k'=%d\n", g.num_edited);
    // dump_graph(g);

    // 2. Bound
    TRACE("  [lower-bound] %s\n", "start");
    auto lb = lower_bounder->lower_bound(g);
    TRACE("  [lower-bound] end   lb=%d\n", lb);
    if (best <= g.num_edited + lb) {
      // branch pruning
      TRACE("  PRUNING BY lower-bound%s\n", "");
      return;
    }

    TRACE("  [upper-bound] start%s\n", "");
    auto ub = upper_bounder->upper_bound(g, lb, cert);
    TRACE("  [upper-bound] end   ub=%d\n", ub);

    if (lb == ub) {
      // found a solution
      best = g.num_edited + lb;
      TRACE("[UPDATE] best=%d\n", best);
      return;
    }

    // 3. Branch
    TRACE("  [branch] start k=%d\n", parent.num_edited);
    auto br = brancher->branch(g);
    TRACE("  [branch] end   #branches=%lu\n", br.size());

    for (auto &e : br) {
      G h(g);
      TRACE("    [edit] k=%d, e=(%d,%d)\n", parent.num_edited, e.first, e.second);
      h.edit_edge(e.first, e.second);
      search(h);
      if (best <= g.num_edited + lb) {
        // found a partially optimum solution
        TRACE("    [exit] k=%d\n", parent.num_edited);
        break;
      }
    }

    TRACE("[search] end: k=%d\n", parent.num_edited);
  }
};

}  // namespace cep
}  // namespace mog