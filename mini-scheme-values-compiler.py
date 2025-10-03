import re
from enum import IntEnum
from typing import Any, Dict, List, Optional, Union, cast

# -----------------------------------------------------------------------------
# 0. 类型别名 (Type Aliases)
# -----------------------------------------------------------------------------

# Lisp 运行时可以操作的值的类型
Value = Union[int, str, List[Any], "ObjClosure", "ObjFunction"]

# 解析后的 S-表达式的类型
SExpression = Union[int, str, List["SExpression"]]


# -----------------------------------------------------------------------------
# 1. 字节码定义 (Opcodes) - 【已修改】
# -----------------------------------------------------------------------------
class OpCode(IntEnum):
    OP_CONSTANT = 1
    OP_POP = 2
    OP_DEFINE_GLOBAL = 3
    OP_GET_GLOBAL = 4
    OP_SET_GLOBAL = 5
    OP_GET_LOCAL = 6
    OP_SET_LOCAL = 7
    OP_ADD = 8
    OP_SUBTRACT = 9
    OP_LIST = 10
    OP_LIST_REF = 11
    OP_CALL = 12
    OP_CLOSURE = 13
    OP_RETURN = 14
    # 新增: 多返回值相关指令
    OP_VALUES = 15
    OP_CALL_WITH_VALUES = 16


# -----------------------------------------------------------------------------
# 2. 核心运行时数据结构 - 【已简化】
# -----------------------------------------------------------------------------
class Chunk:
    """存放一个函数编译后的字节码和常量。"""

    def __init__(self) -> None:
        self.code: bytearray = bytearray()
        self.constants: List[Value] = []

    def write(self, byte: int) -> None:
        self.code.append(byte)

    def add_constant(self, value: Value) -> int:
        self.constants.append(value)
        return len(self.constants) - 1


class Obj:
    """所有堆对象的基类。"""

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}>"


class ObjFunction(Obj):
    """函数“模板”，包含字节码，但不包含捕获状态。"""

    def __init__(self, name: str, arity: int) -> None:
        self.name: str = name
        self.arity: int = arity
        self.chunk: Chunk = Chunk()

    def __repr__(self) -> str:
        return f"<fn {self.name}/{self.arity}>"


class ObjClosure(Obj):
    """真正的可调用对象。它包装了一个函数模板。"""

    def __init__(self, function: ObjFunction) -> None:
        self.function: ObjFunction = function

    def __repr__(self) -> str:
        return f"<closure {self.function.name}/{self.function.arity}>"


# -----------------------------------------------------------------------------
# 3. 编译器 (Compiler) - 【已修改】
# -----------------------------------------------------------------------------
class Compiler:
    # 编译时的局部变量表示
    class Local:
        def __init__(self, name: str, depth: int) -> None:
            self.name: str = name
            self.depth: int = depth

    def __init__(self, enclosing: Optional["Compiler"] = None) -> None:
        self.enclosing: Optional["Compiler"] = enclosing
        self.function: ObjFunction = ObjFunction("<script>", 0)
        self.scope_depth: int = 0
        self.locals: List[Compiler.Local] = [self.Local("", 0)]

    def compile(self, program_str: str) -> ObjFunction:
        expressions = self._parse(program_str)
        last_expr_index = len(expressions) - 1
        for i, expr in enumerate(expressions):
            self.compile_expression(expr)
            is_define = (
                isinstance(expr, list)
                and expr
                and isinstance(expr[0], str)
                and expr[0] == "define"
            )
            if not is_define and i < last_expr_index:
                self.emit_byte(OpCode.OP_POP)

        return self.end_compiler()

    def compile_expression(self, expr: SExpression) -> None:
        if not isinstance(expr, list):
            if isinstance(expr, int):
                self.emit_constant(expr)
            else:  # Symbol (str)
                self.named_variable(expr)
        else:
            if not expr:
                return
            op = expr[0]
            if not isinstance(op, str):
                self.compile_call(expr)
                return

            # 特殊形式
            if op == "define":
                self.compile_expression(expr[2])
                self.define_variable(cast(str, expr[1]))
            elif op == "lambda":
                self.compile_lambda(expr)
            elif op == "list":
                for item in expr[1:]:
                    self.compile_expression(item)
                self.emit_bytes(OpCode.OP_LIST, len(expr) - 1)
            # 新增: 多值处理形式
            elif op == "values":
                arg_count = len(expr) - 1
                for item in expr[1:]:
                    self.compile_expression(item)
                self.emit_bytes(OpCode.OP_VALUES, arg_count)
            elif op == "call-with-values":
                # 编译 producer 和 consumer
                self.compile_expression(expr[1])
                self.compile_expression(expr[2])
                self.emit_byte(OpCode.OP_CALL_WITH_VALUES)
            # 函数调用
            else:
                self.compile_call(expr)

    def compile_call(self, expr: List[SExpression]) -> None:
        op = expr[0]
        # 内置函数特殊处理
        if isinstance(op, str) and op in ["+", "-"]:
            self.compile_expression(expr[1])
            self.compile_expression(expr[2])
            if op == "+":
                self.emit_byte(OpCode.OP_ADD)
            if op == "-":
                self.emit_byte(OpCode.OP_SUBTRACT)
        elif isinstance(op, str) and op == "list-ref":
            self.compile_expression(expr[1])
            self.compile_expression(expr[2])
            self.emit_byte(OpCode.OP_LIST_REF)
        else:  # 用户自定义函数或lambda调用
            self.compile_expression(op)
            for arg in expr[1:]:
                self.compile_expression(arg)
            self.emit_bytes(OpCode.OP_CALL, len(expr) - 1)

    def compile_lambda(self, expr: List[SExpression]) -> None:
        lambda_compiler = Compiler(enclosing=self)
        lambda_compiler.function.name = "<lambda>"
        lambda_compiler.begin_scope()

        params = cast(List[str], expr[1])
        lambda_compiler.function.arity = len(params)
        for param in params:
            lambda_compiler.declare_variable(param)
            lambda_compiler.mark_initialized()

        body_expressions = expr[2:]
        for i, body_expr in enumerate(body_expressions):
            lambda_compiler.compile_expression(body_expr)
            if i < len(body_expressions) - 1:
                lambda_compiler.emit_byte(OpCode.OP_POP)

        function = lambda_compiler.end_compiler()

        idx = self.current_chunk().add_constant(function)
        self.emit_bytes(OpCode.OP_CLOSURE, idx)

    def named_variable(self, name: str) -> None:
        arg = self.resolve_local(name)
        if arg != -1:
            self.emit_bytes(OpCode.OP_GET_LOCAL, arg)
            return

        idx = self.current_chunk().add_constant(name)
        self.emit_bytes(OpCode.OP_GET_GLOBAL, idx)

    def resolve_local(self, name: str) -> int:
        for i in range(len(self.locals) - 1, -1, -1):
            local = self.locals[i]
            if name == local.name:
                return i
        return -1

    def declare_variable(self, name: str) -> None:
        if self.scope_depth == 0:
            return
        self.locals.append(self.Local(name, -1))

    def mark_initialized(self) -> None:
        if self.scope_depth == 0:
            return
        self.locals[-1].depth = self.scope_depth

    def define_variable(self, name: str) -> None:
        if self.scope_depth > 0:
            self.mark_initialized()
            return
        idx = self.current_chunk().add_constant(name)
        self.emit_bytes(OpCode.OP_DEFINE_GLOBAL, idx)

    def begin_scope(self) -> None:
        self.scope_depth += 1

    def end_scope(self) -> None:
        self.scope_depth -= 1
        while self.locals and self.locals[-1].depth > self.scope_depth:
            self.emit_byte(OpCode.OP_POP)
            self.locals.pop()

    def end_compiler(self) -> ObjFunction:
        chunk = self.current_chunk()
        if not chunk.code or chunk.code[-2] != OpCode.OP_VALUES:
             self.emit_byte(OpCode.OP_RETURN)
        return self.function

    def emit_byte(self, byte: Union[int, OpCode]) -> None:
        self.current_chunk().write(int(byte))

    def emit_bytes(self, b1: Union[int, OpCode], b2: int) -> None:
        self.emit_byte(b1)
        self.emit_byte(b2)

    def emit_constant(self, value: Value) -> None:
        idx = self.current_chunk().add_constant(value)
        self.emit_bytes(OpCode.OP_CONSTANT, idx)

    def current_chunk(self) -> Chunk:
        return self.function.chunk

    def _parse(self, program: str) -> List[SExpression]:
        program = re.sub(r";.*", "", program)
        tokens = program.replace("(", " ( ").replace(")", " ) ").split()
        expressions: List[SExpression] = []
        while tokens:
            expressions.append(self._read_from_tokens(tokens))
        return expressions

    def _read_from_tokens(self, tokens: List[str]) -> SExpression:
        if not tokens:
            raise SyntaxError("Unexpected EOF while reading")
        token = tokens.pop(0)
        if token == "(":
            expr_list: List[SExpression] = []
            while tokens and tokens[0] != ")":
                expr_list.append(self._read_from_tokens(tokens))
            if not tokens:
                raise SyntaxError("Unmatched '('")
            tokens.pop(0)
            return expr_list
        elif token == ")":
            raise SyntaxError("Unexpected ')'")
        else:
            try:
                return int(token)
            except ValueError:
                return str(token)

# -----------------------------------------------------------------------------
# 4. 虚拟机 (VM) - 【已修改】
# -----------------------------------------------------------------------------
class CallFrame:
    def __init__(self, closure: ObjClosure, ip: int, slots_start: int) -> None:
        self.closure: ObjClosure = closure
        self.ip: int = ip
        self.slots_start: int = slots_start


class VM:
    def __init__(self) -> None:
        self.stack: List[Value] = []
        self.frames: List[CallFrame] = []
        self.globals: Dict[str, Value] = {}
        self.pending_consumer: Optional[ObjClosure] = None # 新增: 用于C-W-V
        self.debug_trace: bool = False

    def _log_stack(self) -> None:
        if not self.debug_trace: return
        print("          | ", end="")
        if not self.stack: print("[ empty ]"); return
        for i, value in enumerate(self.stack):
            is_in_current_frame = False
            if self.frames and i >= self.current_frame().slots_start:
                is_in_current_frame = True
            print(f"[{value}] " if is_in_current_frame else f"<{value}> ", end="")
        print()

    def _log_instruction(self) -> None:
        if not self.debug_trace: return
        frame = self.current_frame()
        ip = frame.ip
        chunk = frame.closure.function.chunk
        instruction = OpCode(chunk.code[ip])
        print(f"{ip:04d} ", end="")
        if instruction in [OpCode.OP_CONSTANT, OpCode.OP_DEFINE_GLOBAL, OpCode.OP_GET_GLOBAL, OpCode.OP_CLOSURE]:
            const_idx = chunk.code[ip + 1]
            const_val = chunk.constants[const_idx]
            print(f"{instruction.name:<18} {const_idx:4} '{const_val}'")
        elif instruction in [OpCode.OP_GET_LOCAL, OpCode.OP_SET_LOCAL, OpCode.OP_CALL, OpCode.OP_LIST, OpCode.OP_VALUES]:
            operand = chunk.code[ip + 1]
            print(f"{instruction.name:<18} {operand:4}")
        else:
            print(f"{instruction.name}")

    def run(self, function: ObjFunction, debug_trace: bool = False) -> Optional[Value]:
        self.debug_trace = debug_trace
        if self.debug_trace: print("\n--- VM Trace Start ---")

        closure = ObjClosure(function)
        self.push(closure)
        self.call(closure, 0)

        last_result: Optional[Value] = None
        while self.frames:
            frame = self.current_frame()
            if self.debug_trace:
                self._log_stack()
                self._log_instruction()

            instruction = OpCode(self.read_byte(frame))

            if instruction == OpCode.OP_CONSTANT:
                self.push(self.read_constant(frame))
            elif instruction == OpCode.OP_POP:
                last_result = self.pop()
            elif instruction == OpCode.OP_DEFINE_GLOBAL:
                name = cast(str, self.read_constant(frame))
                self.globals[name] = self.peek(0)
                self.pop()
            elif instruction == OpCode.OP_GET_GLOBAL:
                name = cast(str, self.read_constant(frame))
                value = self.globals.get(name)
                if value is None: raise NameError(f"Undefined global variable '{name}'")
                self.push(value)
            elif instruction == OpCode.OP_GET_LOCAL:
                self.push(self.stack[frame.slots_start + self.read_byte(frame)])
            elif instruction == OpCode.OP_ADD:
                b, a = cast(int, self.pop()), cast(int, self.pop())
                self.push(a + b)
            elif instruction == OpCode.OP_SUBTRACT:
                b, a = cast(int, self.pop()), cast(int, self.pop())
                self.push(a - b)
            elif instruction == OpCode.OP_LIST:
                count = self.read_byte(frame)
                items = self.stack[-count:]
                self.stack = self.stack[:-count]
                self.push(items)
            elif instruction == OpCode.OP_LIST_REF:
                index, lst = cast(int, self.pop()), cast(List[Value], self.pop())
                self.push(lst[index])
            elif instruction == OpCode.OP_CALL:
                arg_count = self.read_byte(frame)
                self.call(cast(ObjClosure, self.peek(arg_count)), arg_count)
            elif instruction == OpCode.OP_CLOSURE:
                function = cast(ObjFunction, self.read_constant(frame))
                self.push(ObjClosure(function))
            elif instruction == OpCode.OP_RETURN:
                result = self.pop()
                if self.debug_trace: print(f"--- Return from {frame.closure.function.name} with value: {result} ---")
                self.frames.pop()
                if not self.frames:
                    if self.debug_trace: print("--- VM Trace End ---")
                    return result
                self.stack = self.stack[: frame.slots_start]
                self.push(result)
            elif instruction == OpCode.OP_CALL_WITH_VALUES:
                consumer = cast(ObjClosure, self.pop())
                producer = cast(ObjClosure, self.pop())
                self.pending_consumer = consumer
                if self.debug_trace: print(f"--- Starting CALL-WITH-VALUES, producer: {producer.function.name} ---")
                self.call(producer, 0) # 调用生产者
            elif instruction == OpCode.OP_VALUES:
                value_count = self.read_byte(frame)
                consumer = self.pending_consumer
                if consumer is None:
                    raise RuntimeError("`(values)` can only be used inside a producer for `call-with-values`")
                self.pending_consumer = None
                
                if self.debug_trace: print(f"--- Producer returned {value_count} values, now calling consumer: {consumer.function.name} ---")

                # 从生产者返回，并准备调用消费者
                results = self.stack[-value_count:] if value_count > 0 else []
                self.frames.pop() # 弹出生产者的帧
                self.stack = self.stack[:frame.slots_start] # 清理生产者栈

                # 准备消费者调用
                self.push(consumer)
                self.stack.extend(results)
                self.call(consumer, value_count)


        if self.debug_trace: print("--- VM Trace End ---")
        return last_result

    def call(self, callee: ObjClosure, arg_count: int) -> None:
        if not isinstance(callee, ObjClosure):
            raise TypeError(f"'{type(callee).__name__}' is not callable")

        if callee.function.arity != arg_count:
            raise TypeError(
                f"Expected {callee.function.arity} args, got {arg_count} "
                f"for function <{callee.function.name}>"
            )

        if self.debug_trace:
            print(f"\n--- Entering frame for {callee.function.name}/{callee.function.arity} ---")

        frame = CallFrame(
            closure=callee, ip=0, slots_start=len(self.stack) - arg_count - 1
        )
        self.frames.append(frame)

    def push(self, value: Value) -> None: self.stack.append(value)
    def pop(self) -> Value: return self.stack.pop()
    def peek(self, distance: int) -> Value: return self.stack[-1 - distance]
    def read_byte(self, frame: CallFrame) -> int:
        byte = frame.closure.function.chunk.code[frame.ip]
        frame.ip += 1
        return byte
    def read_constant(self, frame: CallFrame) -> Value:
        return self.current_chunk().constants[self.read_byte(frame)]
    def current_frame(self) -> CallFrame: return self.frames[-1]
    def current_chunk(self) -> Chunk: return self.current_frame().closure.function.chunk


# -----------------------------------------------------------------------------
# 5. 主执行逻辑
# -----------------------------------------------------------------------------
def run_minischeme_mv(program_str: str, debug_trace: bool = False) -> None:
    compiler = Compiler()
    try:
        main_function = compiler.compile(program_str)
        vm = VM()
        vm.run(main_function, debug_trace=debug_trace)
        print("\n--- Minischeme-MV VM 执行完毕 ---")
        # 打印所有全局变量的值作为最终结果
        for name, value in vm.globals.items():
            print(f"{name}: {value}")
    except (SyntaxError, NameError, TypeError, RuntimeError) as e:
        print(f"\n--- An error occurred ---")
        print(f"Error: {e}")

# -----------------------------------------------------------------------------
# 6. 运行示例代码 - 【已更新】
# -----------------------------------------------------------------------------
if __name__ == "__main__":
    test_suite_code = """
    ;; 测试 1: 基本的多值生产与消费
    ;; 预期结果: test1: 30
    (define test1
      (call-with-values
        (lambda () (values 10 20))
        (lambda (x y) (+ x y))))
    
    ;; 测试 2: 使用已定义的函数
    ;; 预期结果: test2: 2
    (define producer (lambda () (values 5 3)))
    (define consumer (lambda (a b) (- a b)))
    (define test2 (call-with-values producer consumer))
    
    ;; 测试 3: 单返回值情况
    ;; 预期结果: test3: 100
    (define test3
      (call-with-values
        (lambda () (values 100))
        (lambda (z) z)))
    
    ;; 测试 4: 零返回值情况
    ;; 预期结果: test4: 42
    (define test4
      (call-with-values
        (lambda () (values))
        (lambda () 42)))
    
    ;; 测试 5: 消费者利用多值创建列表
    ;; 预期结果: test5: [3, 4]
    (define test5
      (call-with-values
        (lambda () (values 3 4))
        (lambda (x y) (list x y))))
    """

    # 运行并开启详细的执行日志
    run_minischeme_mv(test_suite_code, debug_trace=True)