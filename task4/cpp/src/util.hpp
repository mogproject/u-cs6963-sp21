#pragma once
#include <assert.h>
#include <cstring>
#include <iostream>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <queue>
#include <stack>
#include <sstream>
#include <iomanip>
#include <vector>
#include <algorithm>

#if defined NDEBUG
    #define TRACE( format, ... )
#else
    #define TRACE( format, ... )   printf( "%s(%d) " format, __FUNCTION__,  __LINE__, __VA_ARGS__ )
#endif

// #define exists(m,k) ((m).find(k) != (m).end())

//==================================================================================================
// I/O Support
//==================================================================================================
// printers for STL types
template <typename T> std::ostream & operator<<(std::ostream & stream, std::set<T> const& s);
template <typename A, typename B> std::ostream & operator<<(std::ostream & stream, std::pair<A, B> const& p) { return stream << "(" << p.first << ", " << p.second << ")"; }
template <typename A, typename B> std::ostream & operator<<(std::ostream & stream, std::map<A, B> const& p) { stream << "{"; for (auto &x: p) stream << x.first << ":" << x.second << ", "; return stream << "}"; }
template <typename A, typename B> std::ostream & operator<<(std::ostream & stream, std::unordered_map<A, B> const& p) { stream << "{"; for (auto &x: p) stream << x.first << ":" << x.second << ", "; return stream << "}"; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::vector<T> const& v) { stream << "["; for (auto i = v.begin(); i != v.end(); ++i) { if (i != v.begin()) { stream << ", "; } stream << *i; } return stream << "]"; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::set<T> const& s) { stream << "{"; for (auto &x: s) stream << x << ", "; return stream << "}"; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::unordered_set<T> const& s) { stream << "{"; for (auto &x: s) stream << x << ", "; return stream << "}"; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::queue<T> & q) { std::vector<T> v; for (;!q.empty();q.pop()) v.push_back(q.front()); for (auto &x: v) q.push(x); return stream << v; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::stack<T> & s) { std::vector<T> v; for (;!s.empty();s.pop()) v.push_back(s.top()); for (auto it=v.rbegin();it!=v.rend();++it) s.push(*it); return stream << v; }
template <typename T> std::ostream & operator<<(std::ostream & stream, std::priority_queue<T> & q) { std::vector<T> v; for (;!q.empty();q.pop()) v.push_back(q.top()); for (auto &x: v) q.push(x); return stream << v; }

template <typename T> void print(T t) { std::cout << t << std::endl; }
template <typename T> void print(T t, size_t n) { for (int i = 0; i < n; ++i) std::cout << (i > 0 ? " " : "") << t[i]; std::cout << std::endl; }
template <typename T> void print(T t, size_t n, size_t m) { for (int i = 0; i < n; ++i) print(t[i], m); }

// debug print
template <typename T> void dprint(std::string const& label, T t) { printf("%s: ", label.c_str()); print(t); }

#define timeit auto _s=std::chrono::system_clock::now();
#define timeend {std::chrono::duration<double>_e=std::chrono::system_clock::now()-_s;std::cout<<"Elapsed: "<<_e.count()<<" seconds"<<std::endl;}
