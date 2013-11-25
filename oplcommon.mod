tuple task {
  int period;
  int WCET;
}

tuple job {
  string task;
  int release;
  int deadline;
}

tuple dep {
  string src;
  string dst;
}

tuple buf {
  string src;
  string dst;
  int bufsize;
}

tuple pair {
  string first;
  string second;
}

tuple clique {
  {string} clq;
}

int maxOff = ...;
int hyper = ...;

{string} Tasks = ...;
task TaskProps[Tasks] = ...;
{string} Jobs = ...;
job JobProps[Jobs] = ...;
{dep} Deps = ...;
