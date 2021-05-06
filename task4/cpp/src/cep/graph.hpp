// #pragma once
// #include "../io/parser.hpp"
// #include "../util.hpp"
// #include "vmap.hpp"

// namespace mog {
// namespace cep {

// typedef std::pair<int, int> Edge;

// /**
//  * Graph representation.
//  */
// template <int L>
// struct Graph {
//   static constexpr int const N = L * B;

//   // basic info
//   int num_edited;  // keeps track of the number of edited edges
//   VMap<L> vertices;   // bitmap of the vertices to consider
//   VMap<L> nbr1[N];    // neighbors; i.e., adjacency matrix

//   // additional info (more time-consuming to compute)
//   VMap<L> nbr2[N];  // radius-2 neighbors
//   VMap<L> perm[N];  // editable vertex pairs
//   // unsigned long long num_triangles;  // number of triangles

//   /**
//    * Complexity: O(L)
//    */
//   int degree(int v) const { return nbr1[v].size(); }

//   /**
//    * Returns the number of vertices;
//    *
//    * Complexity: O(L)
//    */
//   // int n() const { return vertices.size(); }

//   /**
//    * Returns the number of edges incident to the given vertices;
//    *
//    * Complexity: O(nL)
//    */
//   int num_incident_edges(VMap<L> const& vs) const {
//     int ret = 0;
//     for (auto v : vs.to_vector()) ret += degree(v);
//     return ret / 2;
//   }

//   bool is_clique(VMap<L> const& vs) const { return 2 * num_incident_edges(vs) == vs.size() * (vs.size() - 1); }

//   bool is_cluster_graph() const {
//     auto cc = components();
//     return std::all_of(cc.begin(), cc.end(), [this](VMap<L> const& c) { return this->is_clique(c); });
//   }

//   // int m() const {
//   //   int ret = 0;
//   //   for (auto v : vertices.to_vector()) ret += degree(v);
//   //   return ret / 2;
//   // }

//   std::vector<int> get_vertices() const { return std::move(vertices.to_vector()); }

//   std::vector<int> get_neighbors(int v) const { return std::move(nbr1[v].to_vector()); }

//   Graph() {}

//   /**
//    * Construct a graph from input.
//    *
//    * Complexity: O(n^2 L)
//    */
//   Graph(mog::io::EdgeListInput const& el) : num_edited(0), vertices(el.n) {
//     // edges
//     for (auto& e : el.edges) {
//       nbr1[e.first] |= e.second;
//       nbr1[e.second] |= e.first;
//     }

//     for (int i = 0; i < el.n; ++i) perm[i] = vertices;
//     compute_r2_neighbors();
//     compute_basic_permissions();
//   }

//   inline void lock(int v, int u) {
//     perm[v] -= u;
//     perm[u] -= v;
//   }

//   void edit_edge(int v, int u) {
//     assert(0 <= v && v < N && 0 <= u && u < N && v != u);
//     assert(perm[v][u] && perm[u][v]);  // edge editing must be allowed

//     ++num_edited;

//     // incrementally compute
//     if (nbr1[v][u]) {
//       // edge deletion
//       // edge v-u : distance 1 -> 2 or >=3
//       if (!(nbr1[v] & nbr1[u]).empty()) {
//         nbr2[v] |= u;
//         nbr2[u] |= v;
//       }

//       for (int k = 0; k < 2; ++k) {
//         auto p = k == 0 ? v : u;
//         auto q = k == 0 ? u : v;
//         auto ws = nbr2[p].to_vector();
//         for (auto w : ws) {                         // pick w such that d(v,w) = 2
//           if (((nbr1[p] & nbr1[w]) - q).empty()) {  // p-u-w is the only distance-2 path
//             // p-w is a permanent non-edge
//             nbr2[p] -= w;
//             nbr2[w] -= p;
//             lock(p, w);
//           }
//         }
//       }

//     } else {
//       // edge addition
//       // edge v-u : distance >=2 -> 1
//       nbr2[v] -= u;
//       nbr2[u] -= v;

//       for (int k = 0; k < 2; ++k) {
//         auto p = k == 0 ? v : u;
//         auto q = k == 0 ? u : v;
//         auto ws = nbr1[q].to_vector();
//         for (auto w : ws) {   // pick q's neighbors
//           if (!nbr1[p][w]) {  // there is no edge between p and w
//             nbr2[p] |= w;
//             nbr2[w] |= p;
//           }
//         }
//       }
//     }

//     // flip edge uv
//     nbr1[v] ^= u;
//     nbr1[u] ^= v;

//     // lock v and u
//     lock(v, u);
//   }
//   // /**
//   //  * Construct a graph with selected vertices. The vertices must induce a component.
//   //  *
//   //  * Complexity: O(n^2 L)
//   //  */
//   // Graph(Graph const* parent, VMap<L> vertices) : parent(parent), vertices(vertices) {
//   //   lower_bound = parent->lower_bound;
//   //   upper_bound = parent->upper_bound;

//   //   for (auto v : vertices.to_vector()) {
//   //     nbr1[v] = parent->nbr1[v];
//   //     nbr2[v] = parent->nbr2[v];
//   //     perm[v] = parent->perm[v];
//   //   }
//   //   compute_num_triangles();
//   // }

//   /**
//    * Construct a graph with one edge modification.
//    *
//    * Complexity: O(nL)
//    */
//   // Graph(Graph const* parent, int v, int u) : parent(parent), vertices(parent->vertices) {
//   //   assert(0 <= v && v < N && 0 <= u && u < N && v != u);
//   //   assert(parent->perm[v][u] && parent->perm[u][v]);  // edge modification must be allowed

//   //   lower_bound = parent->lower_bound - 1;
//   //   upper_bound = parent->upper_bound - 1;

//   //   for (auto v : vertices.to_vector()) {
//   //     nbr1[v] = parent->nbr1[v];
//   //     nbr2[v] = parent->nbr2[v];
//   //     perm[v] = parent->perm[v];
//   //   }

//   //   // incrementally compute
//   //   if (nbr1[v][u]) {
//   //     // edge deletion
//   //     // edge v-u : distance 1 -> 2 or >=3
//   //     if (!(nbr1[v] & nbr1[u]).empty()) {
//   //       nbr2[v] |= u;
//   //       nbr2[u] |= v;
//   //     }

//   //     for (int k = 0; k < 2; ++k) {
//   //       auto p = k == 0 ? v : u;
//   //       for (auto w : parent->nbr2[p].to_vector()) {  // pick w such that d(v,w) = 2
//   //         if (((nbr1[p] & nbr1[w]) - u).empty()) {    // p-u-w is the only distance-2 path
//   //           // p-w is a permanent non-edge
//   //           nbr2[p] ^= w;
//   //           nbr2[w] ^= p;
//   //           perm[p] -= w;
//   //           perm[w] -= p;
//   //         }
//   //       }
//   //     }
//   //   } else {
//   //     // edge addition
//   //     // edge v-u : distance >=2 -> 1
//   //     nbr2[v] -= u;
//   //     nbr2[u] -= v;

//   //     for (int k = 0; k < 2; ++k) {
//   //       auto p = k == 0 ? v : u;
//   //       auto q = k == 0 ? u : v;
//   //       for (auto w : parent->nbr1[q].to_vector()) {  // pick q's neighbors
//   //         if (!nbr1[p][w]) {                          // there is no edge between p and w
//   //           nbr2[p] |= w;
//   //           nbr2[w] |= p;
//   //         }
//   //       }
//   //     }
//   //   }

//   //   nbr1[v] ^= u;
//   //   nbr1[u] ^= v;
//   //   perm[v] ^= u;
//   //   perm[u] ^= v;
//   // }

//   // /**
//   //  * Construct a graph with multiple edge modifications.
//   //  *
//   //  * Complexity: O(n^2 L)
//   //  */
//   // Graph(Graph const* parent, std::vector<std::pair<int, int>> modifications)
//   //     : parent(parent), vertices(parent->vertices) {
//   //   for (auto v : vertices.to_vector()) {
//   //     nbr1[v] = parent->nbr1[v];
//   //     perm[v] = parent->perm[v];
//   //   }
//   //   for (auto& e : modifications) {
//   //     assert(parent->perm[e.first][e.second] && parent->perm[e.second][e.first]);  // edge modification must be
//   //     allowed nbr1[e.first] ^= e.second;                                                   // flip bit nbr1[e.second]
//   //     ^= e.first; perm[e.first] -= e.second; perm[e.second] -= e.first;
//   //   }

//   //   lower_bound = parent->lower_bound - modifications.size();
//   //   upper_bound = parent->upper_bound - modifications.size();

//   //   // re-compute invariants
//   //   compute_r2_neighbors();
//   //   compute_num_triangles();
//   //   compute_permissions();
//   // }

//   /**
//    * Complexity: O(nL) ?
//    */
//   std::vector<VMap<L>> components() const {
//     std::vector<VMap<L>> ret;
//     VMap<L> visited;
//     for (auto v : get_vertices())
//       if (!visited[v]) {
//         VMap<L> c, q;
//         c |= v;
//         q |= v;
//         visited |= v;

//         while (true) {
//           auto u = q.pop_front();
//           if (u == -1) break;

//           auto frontier = nbr1[u] - visited;
//           if (frontier.empty()) continue;

//           q |= frontier;
//           c |= frontier;
//           visited |= frontier;
//         }

//         ret.push_back(c);
//       }
//     return ret;
//   }

//   /**
//    * Complexity: O(nL)
//    */
//   std::tuple<int, int, int> find_p3() const {
//     for (auto v : get_vertices()) {
//       if (!nbr2[v].empty()) {
//         auto u = nbr2[v].front();
//         auto w = (nbr1[v] & nbr1[u]).front();
//         return std::make_tuple(v, w, u);
//       }
//     }
//     return std::make_tuple(-1, -1, -1);
//   }

//  private:
//   /**
//    * Complexity: O(n^2 L)
//    */
//   void compute_r2_neighbors() {
//     for (auto v : get_vertices()) {
//       VMap<L> nbr;
//       for (auto u : nbr1[v].to_vector()) nbr |= nbr1[u];
//       nbr -= v;
//       nbr -= nbr1[v];
//       nbr2[v] = nbr;
//     }
//   }

//   /**
//    * Complexity: O(n^2 L)
//    */
//   unsigned long long compute_num_triangles() {
//     unsigned long long x = 0;
//     for (auto v : get_vertices()) {
//       for (auto u : nbr1[v].to_vector()) {
//         if (v < u) { x += (nbr1[v] & nbr1[u]).size(); }
//       }
//     }

//     return x / 3;
//   }

//   /**
//    * Complexity: O(nL)
//    */
//   void compute_basic_permissions() {
//     // Editable vertex pairs must be in distance 1 or 2.
//     for (auto v : get_vertices()) { perm[v] &= nbr1[v] | nbr2[v]; }
//   }

//   // void compute_permissions() {
//   //   // Editable vertex pairs must be in distance 1 or 2.
//   //   for (auto v : get_vertices()) { perm[v] &= nbr1[v] | nbr2[v]; }

//   //   // Edges between true twins cannot be editable.
//   //   for (auto v : get_vertices()) {
//   //     for (auto u : nbr1[v].to_vector()) {
//   //       if (v < u && (nbr1[v] | v) == (nbr1[u] | u)) {
//   //         perm[v] -= u;
//   //         perm[u] -= v;
//   //       }
//   //     }
//   //   }
//   // }
// };

// }  // namespace cep
// }  // namespace mog