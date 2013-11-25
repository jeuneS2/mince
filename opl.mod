using CP;

include "oplcommon.mod";

dvar interval s[j in Jobs]
     in JobProps[j].release..JobProps[j].deadline
     size TaskProps[JobProps[j].task].WCET;

execute {
  cp.param.Workers = 1;
  cp.param.TimeLimit = 900;
}

dexpr int latency = @LATENCY@;

int level = 1;

minimize latency;

constraints {
  // Nothing may overlap
  noOverlap(s);

  // Jobs start after their predecessors have finished
  forall (<src,dst> in Deps) {
    if (level > 0) {
      endBeforeStart(s[src], s[dst]);
    }
  }
}

execute {
  for (j in Jobs)
    writeln("s(",j,") = ", s[j]);
}