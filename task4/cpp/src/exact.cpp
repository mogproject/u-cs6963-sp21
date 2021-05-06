#include "cep/branch.hpp"
#include "data/graph.hpp"
#include "io/parser.hpp"
#include "util.hpp"
#include <fstream>

using namespace std;
using namespace mog::io;
using namespace mog::data;
using namespace mog::cep;

EdgeListInput el;
std::vector<std::pair<int, int>> result;
Reducer r;
LowerBounder lb;
UpperBounder ub;

template <int L>
void do_it() {
  int k = (el.n + Bitmap<0>::B - 1) / Bitmap<0>::B;
  if (k == L) {
    Graph<L> g(el);
    BrancherNaive<L> br;
    BranchAndBound<L> bnb(&r, &lb, &ub, (Brancher<L> *)&br);
    result = bnb.run(g);
  }
}

int main(int argc, char *argv[]) {
#if defined NDEBUG
  el = DIMACSParser::parse(cin);
#else
  if (argc == 1) {
    el = DIMACSParser::parse(cin);
  } else if (argc == 2) {
    ifstream is(argv[1]);
    if (!is) {
      fprintf(stderr, "file open error\n");
      return 2;
    }
    el = DIMACSParser::parse(is);
  } else {
    fprintf(stderr, "Usage: ...\n");
    return 1;
  }
#endif

  if (el.n == 0) return 0;  // do nothing

  if (el.n >= Bitmap<0>::MAX_N) {
    fprintf(stderr, "too large n\n");
    return 1;
  }

  do_it<1>();
  do_it<2>();
  do_it<3>();
  do_it<4>();
  do_it<5>();
  do_it<6>();
  do_it<7>();
  do_it<8>();
  do_it<9>();
  do_it<10>();
  do_it<11>();

  for (auto &e : result) { printf("%d %d\n", e.first + 1, e.second + 1); }
  return 0;
}