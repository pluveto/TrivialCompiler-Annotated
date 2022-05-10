#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <map>
#include <set>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../casting.hpp"
#include "../common.hpp"

// 声明ast中用到的类型，从而让这里不需要include "ast.hpp"。真正需要访问字段的文件里自己include
struct Func;
struct Decl;

struct Inst;
struct IrFunc;
struct Use;
struct MemPhiInst;

// 万能对象……
// Value 泛指一切在指令中被引用的量。包括：常量、全局量、指令
struct Value {
  // value is used by ...
  ilist<Use> uses;
  // tag
  enum class Tag {
#include "op.inc"  // Binary
    Branch,
    Jump,
    Return,  // Control flow
    GetElementPtr,
    Load,
    Store,  // Memory
    Call,
    Alloca,
    Phi,
    MemOp,
    MemPhi,  // 虚拟的MemPhi指令，保证不出现在指令序列中，只出现在BasicBlock::mem_phis中
    Const,
    Global,
    Param,
    Undef,  // Const ~ Undef: Reference
  } tag;

  Value(Tag tag) : tag(tag) {}
  // 添加被使用
  void addUse(Use *u) { uses.insertAtEnd(u); }
  // 删除被使用
  void killUse(Use *u) { uses.remove(u); }

  // 将对自身所有的使用替换成对v的使用
  inline void replaceAllUseWith(Value *v);
  // 调用deleteValue语义上相当于delete掉它，但是按照现在的实现不能直接delete它
  void deleteValue();
};

// 表示某个 Value 被某个 Inst 所引用
struct Use {
  DEFINE_ILIST(Use)

  Value *value;
  Inst *user;

  // 这个构造函数没有初始化prev和next，这没有关系
  // 因为prev和next永远不会从一个Use开始被主动使用，而是在遍历Use链表的时候用到
  // 而既然这个Use已经被加入了一个链表，它的prev和next也就已经被赋值了
  Use(Value *v, Inst *u) : value(v), user(u) {
    if (v) v->addUse(this);
  }

  // 没有必要定义移动构造函数/拷贝运算符，语义没有区别
  // 一般不会用到它们，只在类似vector内部构造临时变量又析构的场景中用到
  Use(const Use &rhs) : value(rhs.value), user(rhs.user) {
    if (value) value->addUse(this);
  }
  Use &operator=(const Use &rhs) {
    if (this != &rhs) {
      assert(user == rhs.user);
      set(rhs.value);
    }
    return *this;
  }

  // 注意不要写.value = xxx, 而是用.set(xxx), 因为需要记录被use的关系
  void set(Value *v) {
    if (value) value->killUse(this);
    value = v;
    if (v) v->addUse(this);
  }

  ~Use() {
    if (value) value->killUse(this);
  }
};

void Value::replaceAllUseWith(Value *v) {
  // head->set会将head从链表中移除
  while (uses.head) uses.head->set(v);
}

struct IrProgram {
  ilist<IrFunc> func;
  std::vector<Decl *> glob_decl;

  friend std::ostream &operator<<(std::ostream &os, const IrProgram &dt);
};

std::ostream &operator<<(std::ostream &os, const IrProgram &dt);

struct BasicBlock {
  DEFINE_ILIST(BasicBlock)
  std::vector<BasicBlock *> pred;
  BasicBlock *idom;
  std::unordered_set<BasicBlock *> dom_by;  // 支配它的节点集
  std::vector<BasicBlock *> doms;           // 它支配的节点集
  u32 dom_level;                            // dom树中的深度，根深度为0
  bool vis;  // 各种算法中用到，标记是否访问过，算法开头应把所有vis置false(调用IrFunc::clear_all_vis)
  ilist<Inst> insts;
  ilist<Inst> mem_phis;  // 元素都是MemPhiInst

  inline std::array<BasicBlock *, 2> succ();
  inline std::array<BasicBlock **, 2> succ_ref();  // 想修改succ时使用
  inline bool valid();
};

struct IrFunc {
  DEFINE_ILIST(IrFunc)
  Func *func;
  ilist<BasicBlock> bb;
  // functions called by this function
  std::set<IrFunc *> callee_func;
  // functions calling this function
  std::set<IrFunc *> caller_func;
  bool builtin;
  bool load_global;
  // has_side_effect: 修改了全局变量/传入的数组参数，或者调用了has_side_effect的函数
  // no side effect函数的没有user的调用可以删除
  bool has_side_effect;
  bool can_inline;

  // pure函数的参数相同的调用可以删除
  bool pure() const { return !(load_global || has_side_effect); }

  // 将所有bb的vis置false
  void clear_all_vis() {
    for (BasicBlock *b = bb.head; b; b = b->next) b->vis = false;
  }
};

struct ConstValue : Value {
  DEFINE_CLASSOF(Value, p->tag == Tag::Const);
  const i32 imm;

  static std::unordered_map<i32, ConstValue *> POOL;

  // 根据常量值在常量池获取对应的 Value 对象
  static ConstValue *get(i32 imm) {
    auto [it, inserted] = POOL.insert({imm, nullptr});
    if (inserted) it->second = new ConstValue(imm);
    return it->second;
  }

 private:
  // use ConstValue::get instead
  explicit ConstValue(i32 imm) : Value(Tag::Const), imm(imm) {}
};

// 表示一个全局量的声明，例如全局变量。和 ParamRef 一个类型的玩意儿，名字不同罢了。
struct GlobalRef : Value {
  DEFINE_CLASSOF(Value, p->tag == Tag::Global);
  Decl *decl;

  GlobalRef(Decl *decl) : Value(Tag::Global), decl(decl) {}
};

struct ParamRef : Value {
  DEFINE_CLASSOF(Value, p->tag == Tag::Param);
  Decl *decl;

  ParamRef(Decl *decl) : Value(Tag::Param), decl(decl) {}
};

struct UndefValue : Value {
  DEFINE_CLASSOF(Value, p->tag == Tag::Undef);

  UndefValue() : Value(Tag::Undef) {}
  // 这是一个全局可变变量，不过反正也不涉及多线程，不会有冲突
  static UndefValue INSTANCE;
};

struct Inst : Value {
  DEFINE_CLASSOF(Value, Tag::Add <= p->tag && p->tag <= Tag::MemPhi);
  // instruction linked list
  DEFINE_ILIST(Inst)
  // 指令所属块
  BasicBlock *bb;

  // insert this inst before `insertBefore`
  Inst(Tag tag, Inst *insertBefore) : Value(tag), bb(insertBefore->bb) 
  {
     bb->insts.insertBefore(this, insertBefore); 
     }

  // insert this inst at the end of `insertAtEnd`
  Inst(Tag tag, BasicBlock *insertAtEnd) : Value(tag), bb(insertAtEnd) 
  { 
    bb->insts.insertAtEnd(this); 
    }

  // 只初始化tag，没有加入到链表中，调用者手动加入
  Inst(Tag tag) : Value(tag) {}

  // 返回的指针对是一个左闭右开区间，表示这条指令的所有操作数，.value可能为空
  std::pair<Use *, Use *> operands();

  inline bool has_side_effect();
};

struct BinaryInst : Inst {
  DEFINE_CLASSOF(Value, Tag::Add <= p->tag && p->tag <= Tag::Or);
  // operands
  // loop unroll pass里用到了lhs和rhs的摆放顺序，不要随便修改
  Use lhs;
  Use rhs;

  BinaryInst(Tag tag, Value *lhs, Value *rhs, BasicBlock *insertAtEnd)
      : Inst(tag, insertAtEnd), lhs(lhs, this), rhs(rhs, this) {}

  BinaryInst(Tag tag, Value *lhs, Value *rhs, Inst *insertBefore)
      : Inst(tag, insertBefore), lhs(lhs, this), rhs(rhs, this) {}

  bool rhsCanBeImm() {
    // Add, Sub, Rsb, Mul, Div, Mod, Lt, Le, Ge, Gt, Eq, Ne, And, Or
    return (tag >= Tag::Add && tag <= Tag::Rsb) || (tag >= Tag::Lt && tag <= Tag::Or);
  }

  constexpr static const char *LLVM_OPS[14] = {
      /* Add = */ "add",
      /* Sub = */ "sub",
      /* Rsb = */ nullptr,
      /* Mul = */ "mul",
      /* Div = */ "sdiv",
      /* Mod = */ "srem",
      /* Lt = */ "icmp slt",
      /* Le = */ "icmp sle",
      /* Ge = */ "icmp sge",
      /* Gt = */ "icmp sgt",
      /* Eq = */ "icmp eq",
      /* Ne = */ "icmp ne",
      /* And = */ "and",
      /* Or = */ "or",
  };

  constexpr static std::pair<Tag, Tag> swapableOperators[11] = {
      {Tag::Add, Tag::Add}, {Tag::Sub, Tag::Rsb}, {Tag::Mul, Tag::Mul}, {Tag::Lt, Tag::Gt},
      {Tag::Le, Tag::Ge},   {Tag::Gt, Tag::Lt},   {Tag::Ge, Tag::Le},   {Tag::Eq, Tag::Eq},
      {Tag::Ne, Tag::Ne},   {Tag::And, Tag::And}, {Tag::Or, Tag::Or},
  };

  bool swapOperand() {
    for (auto [before, after] : swapableOperators) {
      if (tag == before) {
        // note:
        // Use是被pin在内存中的，不能直接swap它们。如果未来希望这样做，需要实现配套的设施，基本上就是把下面的逻辑在构造函数/拷贝运算符中实现
        tag = after;
        Value *l = lhs.value, *r = rhs.value;
        l->killUse(&lhs);
        r->killUse(&rhs);
        l->addUse(&rhs);
        r->addUse(&lhs);
        std::swap(lhs.value, rhs.value);
        return true;
      }
    }
    return false;
  }

  Value *optimizedValue() {
    // imm on rhs
    if (auto r = dyn_cast<ConstValue>(rhs.value)) {
      switch (tag) {
        case Tag::Add:
        case Tag::Sub:
          return r->imm == 0 ? lhs.value : nullptr;  // ADD or SUB 0
        case Tag::Mul:
          if (r->imm == 0) return ConstValue::get(0);  // MUL 0
          [[fallthrough]];
        case Tag::Div:
          if (r->imm == 1) return lhs.value;  // MUL or DIV 1
        case Tag::Mod:
          return r->imm == 1 ? ConstValue::get(0) : nullptr;  // MOD 1
        case Tag::And:
          if (r->imm == 0) return ConstValue::get(0);  // AND 0
          return r->imm == 1 ? lhs.value : nullptr;    // AND 1
        case Tag::Or:
          if (r->imm == 1) return ConstValue::get(1);  // OR 1
          return r->imm == 0 ? lhs.value : nullptr;    // OR 0
        default:
          return nullptr;
      }
    }
    return nullptr;
  }
};

struct BranchInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Branch);
  Use cond;
  // true
  BasicBlock *left;
  // false
  BasicBlock *right;

  BranchInst(Value *cond, BasicBlock *left, BasicBlock *right, BasicBlock *insertAtEnd)
      : Inst(Tag::Branch, insertAtEnd), cond(cond, this), left(left), right(right) {}
};

// 无条件跳转
struct JumpInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Jump);
  // 跳转到的地方
  BasicBlock *next;

  JumpInst(BasicBlock *next, 
  // 所属的基本块
  BasicBlock *insertAtEnd) : Inst(Tag::Jump, insertAtEnd), next(next) {}
};

struct ReturnInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Return);
  Use ret;

  ReturnInst(Value *ret, BasicBlock *insertAtEnd) : Inst(Tag::Return, insertAtEnd), ret(ret, this) {}
};

// 访存指令
// + GetElementPtrInst
// + LoadInst
// + StoreInst
struct AccessInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::GetElementPtr || p->tag == Tag::Load || p->tag == Tag::Store);
  // 数组的定义
  Decl *lhs_sym;
  // 若是第一个，则是对应符号的 AllocaInst，否则指向上一个 GetElementPtrInst 指令
  Use arr;
  // 对应维度表达式的根指令
  Use index;
  AccessInst(Inst::Tag tag, Decl *lhs_sym, Value *arr, Value *index, BasicBlock *insertAtEnd)
      : Inst(tag, insertAtEnd), lhs_sym(lhs_sym), arr(arr, this), index(index, this) {}
};

struct GetElementPtrInst : AccessInst {
  DEFINE_CLASSOF(Value, p->tag == Tag::GetElementPtr);
  // 每个元素的大小
  int multiplier;

  // 获取子数组
  // - arr: 若是第一个，则是对应符号的 AllocaInst，否则指向上一个 GetElementPtrInst 指令
  GetElementPtrInst(Decl *lhs_sym, Value *arr, Value *index, int multiplier, BasicBlock *insertAtEnd)
      : AccessInst(Tag::GetElementPtr, lhs_sym, arr, index, insertAtEnd), multiplier(multiplier) {}
};
// 读取某个变量或常量的值
struct LoadInst : AccessInst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Load);
  Use mem_token;  // 由memdep pass计算
  LoadInst(Decl *lhs_sym, Value *arr, Value *index, BasicBlock *insertAtEnd)
      : AccessInst(Tag::Load, lhs_sym, arr, index, insertAtEnd), mem_token(nullptr, this) {}
};
// 存储指令，表示
struct StoreInst : AccessInst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Store);
  Use data;
  StoreInst(Decl *lhs_sym, Value *arr, Value *data, Value *index, BasicBlock *insertAtEnd)
      : AccessInst(Tag::Store, lhs_sym, arr, index, insertAtEnd), data(data, this) {}
};

struct CallInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Call);
  IrFunc *func;
  std::vector<Use> args;
  CallInst(IrFunc *func, BasicBlock *insertAtEnd) : Inst(Tag::Call, insertAtEnd), func(func) {}
};
// 栈变量分配
struct AllocaInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Alloca);

  Decl *sym;
  AllocaInst(Decl *sym, BasicBlock *insertBefore) : Inst(Tag::Alloca, insertBefore), sym(sym) {}
};

// 自动决策指令
struct PhiInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::Phi);
  // 将被从中做出选择的值们 Phi(incoming_values[0], incoming_values[1], ...)
  std::vector<Use> incoming_values;
  // 流入块，即指令所属块的前驱
  std::vector<BasicBlock *> &incoming_bbs() { return bb->pred; }

  /**
   * @brief Construct a new Phi Inst object
   * 
   * @param insertAtFront 所属基本块。将会把 Phi 指令插入到此基本块前
   */
  explicit PhiInst(BasicBlock *insertAtFront) : Inst(Tag::Phi) {
    bb = insertAtFront;
    /*
     * incom bbs:
     * 1. inc1 流入块
     *      \
     *        -> 本块。本块的构成为：this(phi) 作为 bb 的第一个指令 -> bb 的后面的指令
     *      /
     * 2. inc2 流入块
     */
    bb->insts.insertAtBegin(this);
    u32 n = incoming_bbs().size();
    // 增加容量到 n，这是为了预留空间从而 emplace
    incoming_values.reserve(n);
    for (u32 i = 0; i < n; ++i) {

      // 占据空间，以便优化时填入实际的指令

      // 在new PhiInst的时候还不知道它用到的value是什么，先填nullptr，后面再用Use::set填上
      //                      Use < Value,  PhiInst * >
      incoming_values.emplace_back(nullptr, this);
    }
  }
  // 在某指令前插入此指令
  explicit PhiInst(Inst *insertBefore) : Inst(Tag::Phi, insertBefore) {
    u32 n = incoming_bbs().size();

    // 预留
    incoming_values.reserve(n);
    for (u32 i = 0; i < n; ++i) {
      incoming_values.emplace_back(nullptr, this);
    }
  }
};

struct MemOpInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::MemOp);
  Use mem_token;
  LoadInst *load;
  MemOpInst(LoadInst *load, Inst *insertBefore)
      : Inst(Tag::MemOp, insertBefore), mem_token(nullptr, this), load(load) {}
};

// 它的前几个字段和PhiInst是兼容的，所以可以当成PhiInst用(也许理论上有隐患，但是实际上应该没有问题)
// 我不希望让它继承PhiInst，这也许会影响以前的一些对PhiInst的使用
struct MemPhiInst : Inst {
  DEFINE_CLASSOF(Value, p->tag == Tag::MemPhi);
  std::vector<Use> incoming_values;
  std::vector<BasicBlock *> &incoming_bbs() { return bb->pred; }

  // load依赖store和store依赖load两种依赖用到的MemPhiInst不一样
  // 前者的load_or_arr来自于load的数组地址，类型是Decl *，后者的load_or_arr来自于LoadInst
  void *load_or_arr;

  explicit MemPhiInst(void *load_or_arr, BasicBlock *insertAtFront) : Inst(Tag::MemPhi), load_or_arr(load_or_arr) {
    bb = insertAtFront;
    bb->mem_phis.insertAtBegin(this);
    u32 n = incoming_bbs().size();
    incoming_values.reserve(n);
    for (u32 i = 0; i < n; ++i) {
      incoming_values.emplace_back(nullptr, this);
    }
  }
};

bool Inst::has_side_effect() {
  if (isa<BranchInst>(this) || isa<JumpInst>(this) || isa<ReturnInst>(this) || isa<StoreInst>(this)) return true;
  if (auto x = dyn_cast<CallInst>(this); x && x->func->has_side_effect) return true;
  return false;
}

std::array<BasicBlock *, 2> BasicBlock::succ() {
  Inst *end = insts.tail;  // 必须非空
  if (auto x = dyn_cast<BranchInst>(end))
    return {x->left, x->right};
  else if (auto x = dyn_cast<JumpInst>(end))
    return {x->next, nullptr};
  else if (isa<ReturnInst>(end))
    return {nullptr, nullptr};
  else
    UNREACHABLE();
}

std::array<BasicBlock **, 2> BasicBlock::succ_ref() {
  Inst *end = insts.tail;
  if (auto x = dyn_cast<BranchInst>(end))
    return {&x->left, &x->right};
  else if (auto x = dyn_cast<JumpInst>(end))
    return {&x->next, nullptr};
  else if (isa<ReturnInst>(end))
    return {nullptr, nullptr};
  else
    UNREACHABLE();
}
// 当满足: 尾指令跳转指令及其变体时,才返回有效
// 若无效,调用者视情况进行补全
bool BasicBlock::valid() {
  Inst *end = insts.tail;
  return end && (isa<BranchInst>(end) || isa<JumpInst>(end) || isa<ReturnInst>(end));
}
