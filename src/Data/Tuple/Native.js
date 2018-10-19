'use strict';

exports.prjImpl = function projImpl1 (n) {
  return function projImpl2 (t) {
    return t[n];
  };
};

exports.showTupleN = function showTupleN (t) {
  return JSON.stringify(t);
};

exports.t2 = function t21 (a) {
  return function t22 (b) {
    return [a,b];
  };
};

exports.t3 = function t31 (a) {
  return function t32 (b) {
    return function t33 (c) {
      return [a,b,c];
    };
  };
};

exports.t4 = function t41 (a) {
  return function t42 (b) {
    return function t43 (c) {
      return function t44 (d) {
        return [a,b,c,d];
      };
    };
  };
};

exports.t5 = function t51 (a) {
  return function t52 (b) {
    return function t53 (c) {
      return function t54 (d) {
        return function t55 (e) {
          return [a,b,c,d,e];
        };
      };
    };
  };
};

exports.t6 = function t61 (a) {
  return function t62 (b) {
    return function t63 (c) {
      return function t64 (d) {
        return function t65 (e) {
          return function t66 (f) {
            return [a,b,c,d,e,f];
          };
        };
      };
    };
  };
};

exports.t7 = function t71 (a) {
  return function t72 (b) {
    return function t73 (c) {
      return function t74 (d) {
        return function t75 (e) {
          return function t76 (f) {
            return function t77 (g) {
              return [a,b,c,d,e,f,g];
            };
          };
        };
      };
    };
  };
};

exports.t8 = function t81 (a) {
  return function t82 (b) {
    return function t83 (c) {
      return function t84 (d) {
        return function t85 (e) {
          return function t86 (f) {
            return function t87 (g) {
              return function t88 (h) {
                return [a,b,c,d,e,f,g,h];
              };
            };
          };
        };
      };
    };
  };
};

exports.t9 = function t91 (a) {
  return function t92 (b) {
    return function t93 (c) {
      return function t94 (d) {
        return function t95 (e) {
          return function t96 (f) {
            return function t97 (g) {
              return function t98 (h) {
                return function t99 (i) {
                  return [a,b,c,d,e,f,g,h,i];
                };
              };
            };
          };
        };
      };
    };
  };
};
