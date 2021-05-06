#include "parser.hpp"

namespace mog {
namespace io {

std::ostream& operator<<(std::ostream& os, EdgeListInput const& el) {
  os << "EdgeListInput(n=" << el.n << ",edges={";
  int i = 0;
  for (auto e : el.edges) {
    if (i++ > 0) os << ",";
    os << "(" << e.first << "," << e.second << ")";
  }
  return os << "})";
}
}  // namespace cep
}  // namespace mog