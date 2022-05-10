#include "ir.hpp"

#include <unordered_map>

#include "ast.hpp"

void Value::deleteValue() {
  if (auto x = dyn_cast<BinaryInst>(this))
    delete x;
  else if (auto x = dyn_cast<BranchInst>(this))
    delete x;
  else if (auto x = dyn_cast<JumpInst>(this))
    delete x;
  else if (auto x = dyn_cast<ReturnInst>(this))
    delete x;
  else if (auto x = dyn_cast<GetElementPtrInst>(this))
    delete x;
  else if (auto x = dyn_cast<LoadInst>(this))
    delete x;
  else if (auto x = dyn_cast<StoreInst>(this))
    delete x;
  else if (auto x = dyn_cast<CallInst>(this))
    delete x;
  else if (auto x = dyn_cast<AllocaInst>(this))
    delete x;
  else if (auto x = dyn_cast<PhiInst>(this))
    delete x;
  else if (auto x = dyn_cast<MemOpInst>(this))
    delete x;
  else if (auto x = dyn_cast<MemPhiInst>(this))
    delete x;
  else if (auto x = dyn_cast<ConstValue>(this))
    delete x;
  else if (auto x = dyn_cast<GlobalRef>(this))
    delete x;
  else if (auto x = dyn_cast<ParamRef>(this))
    delete x;
  else  // 假定永远只使用UndefValue::INSTANCE，且永远不会调用deleteValue
    UNREACHABLE();
}

// 常量池
// key: 常量 value: 对应的Value对象
std::unordered_map<i32, ConstValue *> ConstValue::POOL;

UndefValue UndefValue::INSTANCE;

std::pair<Use *, Use *> Inst::operands() {
  constexpr std::pair<Use *, Use *> empty{};
  if (auto x = dyn_cast<BinaryInst>(this))
    return {&x->lhs, &x->rhs + 1};
  else if (auto x = dyn_cast<BranchInst>(this))
    return {&x->cond, &x->cond + 1};
  else if (auto x = dyn_cast<ReturnInst>(this))
    return {&x->ret, &x->ret + 1};
  else if (auto x = dyn_cast<GetElementPtrInst>(this))
    return {&x->arr, &x->index + 1};
  else if (auto x = dyn_cast<LoadInst>(this))
    return {&x->arr, &x->mem_token + 1};
  else if (auto x = dyn_cast<StoreInst>(this))
    return {&x->arr, &x->data + 1};
  else if (auto x = dyn_cast<CallInst>(this))
    return {x->args.data(), x->args.data() + x->args.size()};
  else if (auto x = dyn_cast<PhiInst>(this))
    return {x->incoming_values.data(), x->incoming_values.data() + x->incoming_values.size()};
  else if (auto x = dyn_cast<MemOpInst>(this))
    return {&x->mem_token, &x->mem_token + 1};
  else if (auto x = dyn_cast<MemPhiInst>(this))
    return {x->incoming_values.data(), x->incoming_values.data() + x->incoming_values.size()};
  else
    return empty;
}

void print_dims(std::ostream &os, Expr **dims, Expr **dims_end) {
  if (dims == dims_end) {
    os << "i32 ";
  } else {
    os << "[" << dims[0]->result << " x i32] ";
  }
}

void print_flatten_init(std::ostream &os, Expr **dims, Expr **dims_end, Expr **flatten_init, Expr **flatten_init_end) {
  if (dims == dims_end) {
    // last dim
    os << flatten_init[0]->result;
  } else {
    // one or more dims
    os << "[";
    while (flatten_init != flatten_init_end) {
      os << "i32 ";
      os << flatten_init[0]->result;
      if (flatten_init + 1 != flatten_init_end) {
        os << ", ";
      }
      flatten_init++;
    }
    os << "]";
  }
}

// print value according to type
struct pv {
  IndexMapper<Value> &v_index;
  Value *v;
  pv(IndexMapper<Value> &v_index, Value *v) : v_index(v_index), v(v) {}
  friend std::ostream &operator<<(std::ostream &os, const pv &pv) {
    if (auto x = dyn_cast<ConstValue>(pv.v)) {
      os << x->imm;
    } else if (auto x = dyn_cast<GlobalRef>(pv.v)) {
      os << "%_glob_" << x->decl->name;
    } else if (auto x = dyn_cast<ParamRef>(pv.v)) {
      os << "%" << x->decl->name;
    } else if (isa<UndefValue>(pv.v)) {
      os << "undef";
    } else if (isa<MemPhiInst>(pv.v) || isa<MemOpInst>(pv.v)) {
      os << "mem" << pv.v_index.get(pv.v);
    } else if (isa<StoreInst>(pv.v)) {
      os << "store" << pv.v_index.get(pv.v);
    } else {
      os << "%x" << pv.v_index.get(pv.v);
    }
    return os;
  }
};

// output IR
std::ostream &operator<<(std::ostream &os, const IrProgram &p) {
  using std::endl;

  for (auto &d : p.glob_decl) {
    os << "@_" << d->name << " = global ";
    // type
    print_dims(os, d->dims.data(), d->dims.data() + d->dims.size());
    if (d->has_init) {
      print_flatten_init(os, d->dims.data(), d->dims.data() + d->dims.size(), d->flatten_init.data(),
                         d->flatten_init.data() + d->flatten_init.size());
    } else {
      // default 0 initialized
      os << "zeroinitializer" << endl;
    }
    os << endl;
  }

  for (auto f = p.func.head; f != nullptr; f = f->next) {
    const char *decl = f->builtin ? "declare" : "define";
    const char *ret = f->func->is_int ? "i32" : "void";
    os << decl << " " << ret << " @";
    os << f->func->name << "(";
    for (auto &p : f->func->params) {
      if (p.dims.size() == 0) {
        // simple case
        os << "i32 ";
      } else {
        // array arg
        os << "i32 * ";
      }

      os << "%" << p.name;
      if (&p != &f->func->params.back()) {
        // not last element
        os << ", ";
      }
    }
    os << ")";
    if (f->builtin) {
      os << endl;
      continue;
    } else {
      os << " {" << endl;
    }

    os << "_entry:" << endl;
    for (auto &d : p.glob_decl) {
      os << "\t%_glob_" << d->name << " = getelementptr inbounds ";
      print_dims(os, d->dims.data(), d->dims.data() + d->dims.size());
      os << ", ";
      print_dims(os, d->dims.data(), d->dims.data() + d->dims.size());
      os << "* @_" << d->name;
      if (d->dims.empty()) {
        os << ", i32 0" << endl;
      } else {
        os << ", i32 0, i32 0" << endl;
      }
    }
    os << "\tbr label %_0" << endl;

    // bb的标号没有必要用IndexMapper，而且IndexMapper的编号是先到先得，这看着并不是很舒服
    std::map<BasicBlock *, u32> bb_index;
    IndexMapper<Value> v_index;
    for (auto bb = f->bb.head; bb; bb = bb->next) {
      u32 idx = bb_index.size();
      bb_index.insert({bb, idx});
    }
    for (auto bb = f->bb.head; bb; bb = bb->next) {
      u32 index = bb_index.find(bb)->second;
      os << "_" << index << ": ; preds = ";
      for (u32 i = 0; i < bb->pred.size(); ++i) {
        if (i != 0) os << ", ";
        os << "%_" << bb_index.find(bb->pred[i])->second;
      }
      // 这里原来会输出dom info的，现在不输出了，因为现在所有pass结束后dom info不是有效的
      os << endl;
      for (Inst *i = bb->mem_phis.head; i; i = i->next) {
        auto x = static_cast<MemPhiInst *>(i);
        os << "\t; mem" << v_index.get(x) << " = MemPhi ";
        for (u32 j = 0; j < x->incoming_values.size(); ++j) {
          if (j != 0) os << ", ";
          os << "[" << pv(v_index, x->incoming_values[j].value) << ", %_"
             << bb_index.find(x->incoming_bbs()[j])->second << "]";
        }
        os << " for load/arr@" << x->load_or_arr << endl;
      }
      for (auto inst = bb->insts.head; inst != nullptr; inst = inst->next) {
        os << "\t";
        if (auto x = dyn_cast<AllocaInst>(inst)) {
          // temp ptr
          u32 temp = v_index.alloc();
          os << "%t" << temp << " = alloca ";
          print_dims(os, x->sym->dims.data(), x->sym->dims.data() + x->sym->dims.size());
          os << ", align 4" << endl;
          os << "\t" << pv(v_index, inst) << " = getelementptr inbounds ";
          print_dims(os, x->sym->dims.data(), x->sym->dims.data() + x->sym->dims.size());
          os << ", ";
          print_dims(os, x->sym->dims.data(), x->sym->dims.data() + x->sym->dims.size());
          os << "* %t" << temp;
          if (x->sym->dims.empty()) {
            os << ", i32 0" << endl;
          } else {
            os << ", i32 0, i32 0" << endl;
          }
        } else if (auto x = dyn_cast<GetElementPtrInst>(inst)) {
          os << "; getelementptr " << v_index.get(inst) << endl << "\t";
          u32 temp = v_index.alloc();
          os << "%t" << temp << " = mul i32 " << pv(v_index, x->index.value) << ", " << x->multiplier
             << endl;
          os << "\t" << pv(v_index, inst) << " = getelementptr inbounds i32, i32* " << pv(v_index, x->arr.value)
             << ", i32 "
             << "%t" << temp << endl;
        } else if (auto x = dyn_cast<StoreInst>(inst)) {
          os << "; store " << v_index.get(x) << endl << "\t";
          // temp ptr
          u32 temp = v_index.alloc();
          os << "%t" << temp << " = getelementptr inbounds i32, i32";
          os << "* " << pv(v_index, x->arr.value) << ", ";
          os << "i32 " << pv(v_index, x->index.value);
          os << endl;
          os << "\tstore i32 " << pv(v_index, x->data.value) << ", i32* %t" << temp << ", align 4" << endl;
        } else if (auto x = dyn_cast<LoadInst>(inst)) {
          if (x->mem_token.value) {
            os << "; load@" << x << " arr@" << x->arr.value << ", use " << pv(v_index, x->mem_token.value) << endl
               << "\t";
          }
          // temp ptr
          u32 temp = v_index.alloc();
          os << "%t" << temp << " = getelementptr inbounds i32, i32";
          os << "* " << pv(v_index, x->arr.value) << ", ";
          os << "i32 " << pv(v_index, x->index.value);
          os << endl;
          os << "\t" << pv(v_index, inst) << " = load i32, i32* %t" << temp << ", align 4" << endl;
        } else if (auto x = dyn_cast<BinaryInst>(inst)) {
          auto op_name = BinaryInst::LLVM_OPS[(int) x->tag];
          bool conversion = Value::Tag::Lt <= x->tag && x->tag <= Value::Tag::Ne;
          if (conversion) {
            u32 temp = v_index.alloc();
            os << "%t" << temp << " = " << op_name << " i32 " << pv(v_index, x->lhs.value) << ", "
               << pv(v_index, x->rhs.value) << endl;
            os << "\t" << pv(v_index, inst) << " = "
               << "zext i1 "
               << "%t" << temp << " to i32" << endl;
          } else if (x->tag == Value::Tag::Rsb) {
            os << pv(v_index, inst) << " = sub i32 " << pv(v_index, x->rhs.value) << ", "
               << pv(v_index, x->lhs.value) << endl;
          } else {
            os << pv(v_index, inst) << " = " << op_name << " i32 " << pv(v_index, x->lhs.value) << ", "
               << pv(v_index, x->rhs.value) << endl;
          }
        } else if (auto x = dyn_cast<JumpInst>(inst)) {
          os << "br label %_" << bb_index.find(x->next)->second << endl;
        } else if (auto x = dyn_cast<BranchInst>(inst)) {
          // add comment
          os << "; if " << pv(v_index, x->cond.value) << " then _" << bb_index.find(x->left)->second << " else _"
             << bb_index.find(x->right)->second << endl;
          u32 temp = v_index.alloc();
          os << "\t%t" << temp << " = icmp ne i32 " << pv(v_index, x->cond.value) << ", 0" << endl;
          os << "\tbr i1 %t" << temp << ", label %_" << bb_index.find(x->left)->second << ", label %_"
             << bb_index.find(x->right)->second << endl;
        } else if (auto x = dyn_cast<ReturnInst>(inst)) {
          if (x->ret.value) {
            os << "ret i32 " << pv(v_index, x->ret.value) << endl;
          } else {
            os << "ret void" << endl;
          }
        } else if (auto x = dyn_cast<CallInst>(inst)) {
          Func *callee = x->func->func;
          if (callee->is_int) {
            os << pv(v_index, inst) << " = call i32";
          } else {
            os << "call void";
          }
          os << " @" << callee->name << "(";
          for (u32 i = 0; i < x->args.size(); i++) {
            // type
            if (callee->params[i].dims.empty()) {
              // simple
              os << "i32 ";
            } else {
              // array param
              os << "i32 * ";
            }
            // arg
            os << pv(v_index, x->args[i].value);
            if (i + 1 < x->args.size()) {
              // not last element
              os << ", ";
            }
          }
          os << ")" << endl;
        } else if (auto x = dyn_cast<PhiInst>(inst)) {
          os << pv(v_index, inst) << " = phi i32 ";
          for (u32 i = 0; i < x->incoming_values.size(); ++i) {
            if (i != 0) os << ", ";
            os << "[" << pv(v_index, x->incoming_values[i].value) << ", %_"
               << bb_index.find(x->incoming_bbs()[i])->second << "]";
          }
          os << endl;
        } else if (auto x = dyn_cast<MemOpInst>(inst)) {
          os << "; mem" << v_index.get(x) << " for load@" << x->load << ", use " << pv(v_index, x->mem_token.value)
             << endl;
        } else {
          UNREACHABLE();
        }
      }
    }

    os << "}" << endl << endl;
  }

  return os;
}
