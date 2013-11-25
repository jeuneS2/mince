using CP;

include "oplcommon.mod";

dvar interval s[j in Jobs]
     in JobProps[j].release..JobProps[j].deadline
     size TaskProps[JobProps[j].task].WCET;

execute {
  cp.param.Workers = 1;
  cp.param.TimeLimit = 900;
}

int level = 1;

// minimize n_cores;

constraints {
  // Jobs start after their predecessors have finished
  forall (<src,dst> in Deps) {
    if (level > 0) {
      endBeforeStart(s[src], s[dst]);
    }
  }

  // Jobs for which we can imply an ordering
  forall (<j,l> in Ordered) {
    if (level > 0) {
	   startOf(s[l]) >= endOf(s[j]);
    }
  }
  // Jobs for which there is no ordering
  forall (<j,l> in Unordered) {
    if (level > 0) {
	   (startOf(s[j]) >= endOf(s[l])) || (startOf(s[l]) >= endOf(s[j]));
    }
  }
}

execute {
  for (j in Jobs)
    writeln("s(",j,") = ", s[j]);
}