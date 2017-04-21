#include <limits.h>
#include <stdlib.h>

#include <HsFFI.h>

HsInt graph_diameter ( HsInt *nodes, HsInt nodesLen, HsInt *edges, HsInt edgesLen )
{
  // Compute the minimum and maximum node ids
  HsInt lo = HS_INT_MAX, hi = HS_INT_MIN;
  for (int i = 0; i < nodesLen; i++) {
    const int n = nodes[i];
    if (lo > n) lo = n;
    if (hi < n) hi = n;
  }

  // Normalize all the nodes so that the smallest node is zero
  for (int i = 0; i < nodesLen; i++) {
    nodes[i] -= lo;
  }

  // compute the size of an array needed for all the normalized nodes
  // to be valid indexes into it
  const HsInt width = hi - lo + 1;
  unsigned char a[width][width];

  // w is a convenient reference to the weight indexed by offsets into
  // the nodes array.
#define w(i,j) (a[nodes[i]][nodes[j]])

  // Initialize all the paths to "infinity"
  for (int i = 0; i < nodesLen; i++) {
    for (int j = i+1; j < nodesLen; j++) {
      w(i,j) = w(j,i) = UCHAR_MAX;
    }
  }

  // Set path cost where an edge exists to one
  for (int i = 0; i < edgesLen; i+=2) {
    a[edges[i  ]-lo][edges[i+1]-lo] = 1;
    a[edges[i+1]-lo][edges[i]  -lo] = 1;
  }

  // Set path cost from a node to itself to zero
  for (int i = 0; i < nodesLen; i++) {
    w(i,i) = 0;
  }

  for (int k = 0; k < nodesLen; k++) {
    for (int i = 0; i < nodesLen; i++) {
      for (int j = i+1; j < nodesLen; j++) {
        const int l = w(i,k) + w(k,j);
        if (w(i,j) > l) { w(j,i) = w(i,j) = l; }
      }
    }
  }

  // Compute the maximum path cost between any two nodes in the graph
  int res = 0;
  for (int i = 0; i < nodesLen; i++) {
    for (int j = i+1; j < nodesLen; j++) {
      const int l = w(i,j);
      if (l > res) { res = l; }
    }
  }

#undef w

  return res;
}
