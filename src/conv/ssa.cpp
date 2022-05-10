#include "ssa.hpp"

#include "../casting.hpp"
#include "../structure/ast.hpp"

struct SsaContext {
  IrProgram *program;
  // 当前在处理的函数
  IrFunc *func;
  BasicBlock *bb;
  // bb stack for (continue, break)
  std::vector<std::pair<BasicBlock *, BasicBlock *>> loop_stk;
};

// 将 Expr 转换为 SSA 形式，返回值为根指令. 指令呈现树形结构。
Value *convert_expr(SsaContext *ctx, Expr *expr) {

  // 处理二元运算
  if (auto x = dyn_cast<Binary>(expr)) {
    auto lhs = convert_expr(ctx, x->lhs);
    // 处理 Mod，返回以 remainder（余数）为 handle 的 SSA 树
    if (x->tag == Expr::Tag::Mod) {
      auto rhs = convert_expr(ctx, x->rhs);
      // 将 % 转化为等价的简单运算
      // a % b := a - b * a / b (ARM has no MOD instruction)
      auto quotient = new BinaryInst(Value::Tag::Div, lhs, rhs, ctx->bb);
      auto multiple = new BinaryInst(Value::Tag::Mul, rhs, quotient, ctx->bb);
      auto remainder = new BinaryInst(Value::Tag::Sub, lhs, multiple, ctx->bb);
      return remainder;
    } 
    // 处理逻辑与、或，返回一条 Phi 指令
    else if (x->tag == Expr::And || x->tag == Expr::Or) {
      /**
       * 诸如
       *    (expr_lhs) && (expr_rhs) -- rhs
       *      |
       *    after
       * 
       * 需要实现短路功能。
       * 当 expr_lhs 为 true 时，进入 after_bb
       * 当 expr_lhs 为 false 时，进入 rhs_bb
       * 从而引入分支指令
       * 
       * 为了决策去哪个 bb，需要用 Phi 指令。
       */
      auto rhs_bb = new BasicBlock, after_bb = new BasicBlock;
      ctx->func->bb.insertAtEnd(rhs_bb);
      if (x->tag == Expr::And) {
        new BranchInst(lhs, rhs_bb, after_bb, ctx->bb);
      } else {
        auto inv = new BinaryInst(Value::Tag::Eq, lhs, ConstValue::get(0), ctx->bb);
        new BranchInst(inv, rhs_bb, after_bb, ctx->bb);
      }
      // 这里需要pred的大小为2，真正维护pred在最后才做，可以保证是[当前bb, rhs的实际计算bb]的顺序
      // 注意rhs的实际计算bb不一定就是rhs_bb，因为rhs也可能是&& ||
      after_bb->pred.resize(2);
      ctx->bb = rhs_bb;
      auto rhs = convert_expr(ctx, x->rhs);
      new JumpInst(after_bb, ctx->bb); // 从 rhs_bb(ctx->bb) 跳转到 after_bb
      // 将指令追加到函数指令序列
      ctx->func->bb.insertAtEnd(after_bb);
      ctx->bb = after_bb;
      // 创建 Phi 指令
      auto inst = new PhiInst(ctx->bb);
      inst->incoming_values[0].set(lhs);
      inst->incoming_values[1].set(rhs);
      return inst;
    } 
    // 处理其它二元运算
    else {
      auto rhs = convert_expr(ctx, x->rhs);
      // happened to have same tag values
      auto inst = new BinaryInst((Value::Tag)x->tag, lhs, rhs, ctx->bb);
      return inst;
    }
  } 
  // 处理常量
  else if (auto x = dyn_cast<IntConst>(expr)) {
    return ConstValue::get(x->val);
  } 
  // 处理数组索引 a[x] 形式访问，或者普通  a 形式访问符号
  else if (auto x = dyn_cast<Index>(expr)) {
    // 测量形状
    std::vector<Value *> dims;
    // -- 预留空间
    dims.reserve(x->dims.size());
    for (auto &p : x->dims) {
      auto value = convert_expr(ctx, p);
      dims.push_back(value);
    }
    // 如果索引的维度和此符号声明形状一样，则说明是：
    // 1. 直接访问完整的原来的数组，例如定义了 a[2][3]，则用 a[2][0] 来访问属于此种情况
    // 2. 或者，这个符号就不是个数组
    if (x->dims.size() == x->lhs_sym->dims.size()) {
      // access to element
      // 若无形状, 则相当于访问数组的第一个元素，或者不是数组
      // 比如 访问 a 相当于 a[0]
      if (x->dims.empty()) {
        // direct access
        // 直接访问第一个元素        
        auto inst = new LoadInst(x->lhs_sym, x->lhs_sym->value, ConstValue::get(0), ctx->bb);
        //                           ^ 声明的符号          ^ 对应的 AllocaInst
        return inst;
      } 
      // 否则, 则需计算偏移量进行访问
      else {
        // val 是符号 Ast 对应的 Value, 一个数组 Value
        // all levels except last level, emit GetElementPtr
        Value *val = x->lhs_sym->value;
        // 返回的指令
        Inst *res = nullptr;
        // 遍历所有维度，创建指令序列 GetElementPtrInst,GetElementPtrInst,..., LoadInst
        for (u32 i = 0; i < x->dims.size(); i++) {
          int size = i + 1 < x->lhs_sym->dims.size() ? x->lhs_sym->dims[i + 1]->result : 1;
          // 如果不是最后一维, 则需要计算偏移量
          // if (i != lastIdx) where lastIdx = x->dims.size() - 1
          if (i + 1 < x->dims.size()) {
            auto inst = new GetElementPtrInst(x->lhs_sym, val, dims[i], size, ctx->bb);
            res = inst;
            val = inst;
          } else {
            auto inst = new LoadInst(x->lhs_sym, val, dims[i], ctx->bb);
            res = inst;
          }
        }

        return res;
      }
    } 
    // 否则，如果有形状，则是子数组访问，例如定义了 a[2][3]，则用 a[2] 来访问属于此种情况
    else if (x->dims.size()) {
      // access to sub array
      // emit GetElementPtr for each level
      Value *val = x->lhs_sym->value;
      Inst *res = nullptr;
      for (u32 i = 0; i < x->dims.size(); i++) {
        int size = i + 1 < x->lhs_sym->dims.size() ? x->lhs_sym->dims[i + 1]->result : 1;
        auto inst = new GetElementPtrInst(x->lhs_sym, val, dims[i], size, ctx->bb);
        res = inst;
        val = inst;
      }
      return res;
    } 
    // 否则，是直接访问数组首地址，例如定义了 a[2][3]，则用 a 来访问属于此种情况
    else {
      // access to array itself
      auto inst = new GetElementPtrInst(x->lhs_sym, x->lhs_sym->value, ConstValue::get(0), 0, ctx->bb);
      return inst;
    }
  }
  // 处理函数调用
  else if (auto x = dyn_cast<Call>(expr)) {
    // 解析参数
    // must evaluate args before calling
    std::vector<Value *> args;
    args.reserve(x->args.size());
    for (auto &p : x->args) {
      auto value = convert_expr(ctx, p);
      args.push_back(value);
    }
    // 生成调用指令
    auto inst = new CallInst(x->f->val, ctx->bb);
    // 设置指令参数
    // args
    inst->args.reserve(x->args.size());
    for (auto &value : args) {
      inst->args.emplace_back(value, inst);
    }
    return inst;
  }
  return nullptr;
}
// ! Stmt 转 SSA
void convert_stmt(SsaContext *ctx, Stmt *stmt) {
  // 处理声明语句
  if (auto x = dyn_cast<DeclStmt>(stmt)) {
    for (auto &decl : x->decls) {
      // 处理每个声明

      // local variables
      // 创建一条分配指令，分类常量变量
      auto inst = new AllocaInst(&decl, ctx->bb); 
      decl.value = inst; // 指向的 Alloca 指令（后面消除此类指令）

      // handle init expr
      // 若有初始化值
      if (decl.has_init) {
        // 若是有初始化表达式，说明它不是数组初始化值
        if (decl.init.val1) {
          // assign variable to expr
          auto init = convert_expr(ctx, decl.init.val1);
          new StoreInst(&decl, inst, init, ConstValue::get(0), ctx->bb);
        } 
        // 否则是数组初始化. 需要预先填充好 flatten_init
        else {
          // assign each element of flatten_init
          // heuristic: count how many elements are zero
          int num_zeros = 0;
          std::vector<Value *> values;
          values.reserve(decl.flatten_init.size());
          for (u32 i = 0; i < decl.flatten_init.size(); i++) {
            // 获取初始化值
            auto init = convert_expr(ctx, decl.flatten_init[i]);
            values.push_back(init);
            if (auto x = dyn_cast<ConstValue>(init)) {
              if (x->imm == 0) {
                num_zeros++;
              }
            }
          }

          bool emit_memset = false;
          if (num_zeros > 10) {
            // 如果 0 数量很多，则使用 memset 进行初始化
            emit_memset = true;
            auto call_inst = new CallInst(Func::BUILTIN[8].val, ctx->bb);
            call_inst->args.reserve(3);
            // 压入参数:
            // + arr
            // + value
            // + count
            // arr
            call_inst->args.emplace_back(inst, call_inst);
            // ch
            call_inst->args.emplace_back(ConstValue::get(0), call_inst);
            // count = num * 4
            call_inst->args.emplace_back(ConstValue::get(decl.dims[0]->result * 4), call_inst);
          } 

          for (u32 i = 0; i < decl.flatten_init.size(); i++) {
            // skip safely
            if (auto x = dyn_cast<ConstValue>(values[i])) {
              // 跳过 0 的批量初始化
              if (emit_memset && x->imm == 0) {
                continue;
              }
            }

            new StoreInst(&decl, inst, values[i], ConstValue::get(i), ctx->bb);
          }
        }
      }
    }
  }
  // 处理赋值
  else if (auto x = dyn_cast<Assign>(stmt)) {
    // 若 x 是数组, 计算下标
    // evaluate dims first

    // dims 的元素为对应维度表达式的根指令
    std::vector<Value *> dims;
    dims.reserve(x->dims.size());
    for (auto &expr : x->dims) {
      // 依次计算各个下标的根指令
      auto dim = convert_expr(ctx, expr);
      dims.push_back(dim);
    }

    // rhs
    // 获取 rhs 的 Value
    auto rhs = convert_expr(ctx, x->rhs);

    if (x->dims.empty()) {
      new StoreInst(x->lhs_sym, x->lhs_sym->value, rhs, ConstValue::get(0), ctx->bb);
    } else {
      // all levels except last level, emit GetElementPtr
      // 生成 GetElementPtrInst, GetElementPtrInst, ... , StoreInst 指令序列      

      // 存放上次生成的指令
      auto last = x->lhs_sym->value;
      for (u32 i = 0; i < x->dims.size(); i++) {
        int size = i + 1 < x->lhs_sym->dims.size() ? x->lhs_sym->dims[i + 1]->result : 1;
        if (i + 1 < x->dims.size()) {
          auto inst = new GetElementPtrInst(x->lhs_sym, last, dims[i], size, ctx->bb);
          last = inst;
        } else {
          new StoreInst(x->lhs_sym, last, rhs, dims[i], ctx->bb);
        }
      }
    }

  } 
  // 处理条件分支
  else if (auto x = dyn_cast<If>(stmt)) {
    // 1. check `cond`
    // 2. branch to `then` or `else`
    // 3. jump to `end` at the end of `then` and `else`
    auto cond = convert_expr(ctx, x->cond);
    BasicBlock *bb_then = new BasicBlock;
    BasicBlock *bb_else = new BasicBlock;
    BasicBlock *bb_end = new BasicBlock;
    ctx->func->bb.insertAtEnd(bb_then);
    ctx->func->bb.insertAtEnd(bb_else);
    ctx->func->bb.insertAtEnd(bb_end);

    new BranchInst(cond, bb_then, bb_else, ctx->bb);

    // then
    ctx->bb = bb_then;
    convert_stmt(ctx, x->on_true);
    // jump to end bb
    if (!ctx->bb->valid()) {
      new JumpInst(bb_end, ctx->bb);
    }
    // else
    ctx->bb = bb_else;
    if (x->on_false) {
      convert_stmt(ctx, x->on_false);
    }
    // jump to end bb
    if (!ctx->bb->valid()) {
      new JumpInst(bb_end, ctx->bb);
    }

    ctx->bb = bb_end;
  } else if (auto x = dyn_cast<While>(stmt)) {
    // four bb:
    // cond1: loop or end
    // loop: cond2
    // cond2 : loop or end
    // end
    BasicBlock *bb_cond1 = new BasicBlock;
    BasicBlock *bb_loop = new BasicBlock;
    BasicBlock *bb_cond2 = new BasicBlock;
    BasicBlock *bb_end = new BasicBlock;
    ctx->func->bb.insertAtEnd(bb_cond1);
    ctx->func->bb.insertAtEnd(bb_loop);

    // jump to cond1 bb
    new JumpInst(bb_cond1, ctx->bb);

    // cond1
    ctx->bb = bb_cond1;
    auto cond = convert_expr(ctx, x->cond);
    new BranchInst(cond, bb_loop, bb_end, ctx->bb);

    // loop
    ctx->bb = bb_loop;
    ctx->loop_stk.emplace_back(bb_cond2, bb_end);
    convert_stmt(ctx, x->body);
    ctx->loop_stk.pop_back();
    // jump to cond2 bb
    if (!ctx->bb->valid()) {
      new JumpInst(bb_cond2, ctx->bb);
    }

    // cond2
    ctx->func->bb.insertAtEnd(bb_cond2);
    ctx->bb = bb_cond2;
    cond = convert_expr(ctx, x->cond);
    new BranchInst(cond, bb_loop, bb_end, ctx->bb);

    ctx->func->bb.insertAtEnd(bb_end);
    ctx->bb = bb_end;
  } else if (auto x = dyn_cast<Block>(stmt)) {
    for (auto &stmt : x->stmts) {
      convert_stmt(ctx, stmt);
      if (isa<Continue>(stmt) || isa<Break>(stmt) || isa<Return>(stmt)) {
        break;
      }
    }
  } else if (auto x = dyn_cast<Return>(stmt)) {
    if (x->val) {
      auto value = convert_expr(ctx, x->val);
      new ReturnInst(value, ctx->bb);
    } else {
      new ReturnInst(nullptr, ctx->bb);
    }
  } else if (auto x = dyn_cast<ExprStmt>(stmt)) {
    if (x->val) {
      convert_expr(ctx, x->val);
    }
  } else if (isa<Continue>(stmt)) {
    new JumpInst(ctx->loop_stk.back().first, ctx->bb);
  } else if (isa<Break>(stmt)) {
    new JumpInst(ctx->loop_stk.back().second, ctx->bb);
  }
}

// 将程序变成 SSA IR
IrProgram *convert_ssa(Program &p) {
  IrProgram *ret = new IrProgram;
  // 内置函数
  for (Func &builtin : Func::BUILTIN) {
    IrFunc *func = new IrFunc;
    func->builtin = true;
    func->func = &builtin;
    builtin.val = func;
    ret->func.insertAtEnd(func);
  }
  // 全局非内置函数
  for (auto &g : p.glob) {
    // std::variant<Func, Decl>
    //                ^
    //           下标 0 是函数
    if (Func *f = std::get_if<0>(&g)) {
      IrFunc *func = new IrFunc;
      func->builtin = false;
      func->func = f;
      f->val = func;
      ret->func.insertAtEnd(func);
    }
  }
  
  for (auto &g : p.glob) {
    // 生成各个全局函数的 SSA
    if (Func *f = std::get_if<0>(&g)) {
      IrFunc *func = f->val;

      // 函数的入口基本块
      BasicBlock *entryBB = new BasicBlock;
      func->bb.insertAtEnd(entryBB);

      // 函数的参数
      // setup params
      for (auto &p : f->params) {
        if (p.dims.empty()) {
          // 处理非数组阐述
          // alloca for each non-array param
          auto inst = new AllocaInst(&p, entryBB);
          p.value = inst;
          // then copy param into it
          new StoreInst(&p, inst, new ParamRef(&p), ConstValue::get(0), entryBB);
        } else {
          // 数组参数则放个 ParamRef, 它是一个到数组定义 Decl 的指针 (Decl *),
          // there is no need to alloca for array param
          p.value = new ParamRef(&p);
        }
      }
      // Ssa 上下文, 构建 SSA 时的共享状态. 副作用万恶之源
      SsaContext ctx = {ret, func, entryBB};
      for (auto &stmt : f->body.stmts) {
        // 将语句转换为 SSA IR, 追加到函数块
        convert_stmt(&ctx, stmt);
      }
      // 如果函数没有返回值, 则补全生成返回指令
      // add extra return statement to avoid undefined behavior      
      if (!ctx.bb->valid()) {
        // 处理 void/int 返回值
        if (func->func->is_int) {
          new ReturnInst(ConstValue::get(0), ctx.bb);
        } else {
          new ReturnInst(nullptr, ctx.bb);
        }
      }
      // 清空函数中每个基本块的前驱
      for (BasicBlock *bb = func->bb.head; bb; bb = bb->next) {
        bb->pred.clear();
      }
      // 将函数中每个基本块的后继指向其前驱
      for (BasicBlock *bb = func->bb.head; bb; bb = bb->next) {
        for (BasicBlock *x : bb->succ()) {
          // 如果当前基本块的后继存在，则后继的前驱为当前基本块
          //    [cur] ->   [x]
          // 修复双向链接了属于是
          if (x) x->pred.push_back(bb);
        }
      }
    } 
    // 生成各个全局声明的 SSA
    else {
      Decl *d = std::get_if<1>(&g);
      ret->glob_decl.push_back(d);
      d->value = new GlobalRef(d);
    }
  }
  return ret;
}
