#ifndef __MML_AST_IDENTITY_NODE_H__
#define __MML_AST_IDENTITY_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/unary_operation_node.h>

namespace mml {

  class identity_node: public cdk::unary_operation_node {
    public:
      inline identity_node(int lineno, expression_node * argument) :
            cdk::unary_operation_node(lineno, argument) {}

      void accept(basic_ast_visitor *sp, int level) {
        sp->do_identity_node(this, level);
      }

    };

} // mml

#endif 
