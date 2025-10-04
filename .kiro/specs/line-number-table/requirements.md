# Requirements Document

## Introduction

本文档描述了为mini-scheme编译器添加行号表（Line Number Table）功能的需求。行号表将在AST层面保留源代码位置信息，在字节码层面建立字节码偏移量与源代码行号的映射关系，从而改善调试体验和错误报告的准确性。

## Requirements

### Requirement 1

**User Story:** 作为一个开发者，我希望AST节点能够保留源代码的位置信息，这样编译器就能够追踪每个语法结构在源代码中的确切位置。

#### Acceptance Criteria

1. WHEN 解析器创建AST节点 THEN 系统应该为每个有意义的AST节点附加行号和列号信息
2. WHEN AST节点包含位置信息 THEN 该信息应该包括起始行号、起始列号、结束行号和结束列号
3. WHEN 嵌套的AST节点被创建 THEN 子节点应该继承或计算正确的位置信息
4. WHEN 位置信息不可用 THEN 系统应该提供合理的默认值或错误处理
5. WHEN AST被序列化或调试输出 THEN 位置信息应该被包含在输出中

### Requirement 2

**User Story:** 作为一个编译器开发者，我希望在代码生成阶段能够维护字节码偏移量与源代码行号的映射关系，这样虚拟机就能够在运行时提供准确的错误位置。

#### Acceptance Criteria

1. WHEN 编译器开始为新的源代码行生成字节码 THEN 系统应该记录当前字节码偏移量和对应的源代码行号
2. WHEN 字节码生成完成 THEN 系统应该构建一个完整的行号表，包含字节码偏移量到源代码行号的映射
3. WHEN 多个AST节点对应同一源代码行 THEN 行号表应该只记录该行的第一个字节码偏移量
4. WHEN AST节点跨越多行 THEN 行号表应该为每个涉及的行记录适当的字节码偏移量
5. WHEN 编译器优化导致字节码重排 THEN 行号表应该反映实际的字节码布局

### Requirement 3

**User Story:** 作为一个虚拟机开发者，我希望能够根据当前程序计数器（PC）快速查找对应的源代码行号，这样就能在运行时错误中提供准确的位置信息。

#### Acceptance Criteria

1. WHEN 虚拟机需要报告错误位置 THEN 系统应该能够根据当前PC值查找对应的源代码行号
2. WHEN 查找行号 THEN 系统应该找到小于或等于当前PC的最大字节码偏移量对应的行号
3. WHEN PC值超出行号表范围 THEN 系统应该提供合理的错误处理或默认行号
4. WHEN 行号表为空 THEN 系统应该提供适当的回退机制
5. WHEN 查找操作 THEN 系统应该提供高效的查找算法（如二分查找）

### Requirement 4

**User Story:** 作为一个mini-scheme用户，我希望在程序出现运行时错误时能够看到准确的源代码位置信息，这样我就能快速定位和修复问题。

#### Acceptance Criteria

1. WHEN 运行时发生错误 THEN 错误消息应该包含准确的源代码行号和列号
2. WHEN 错误发生在函数调用中 THEN 错误消息应该显示调用栈中每一层的位置信息
3. WHEN 错误消息显示位置信息 THEN 格式应该清晰易读，如"at line 15, column 8"
4. WHEN 位置信息不可用 THEN 错误消息应该明确指出位置信息缺失
5. WHEN 调试模式启用 THEN 系统应该提供更详细的位置信息和上下文

### Requirement 5

**User Story:** 作为一个编译器维护者，我希望行号表功能能够与现有的追踪和调试系统集成，这样就能提供统一的调试体验。

#### Acceptance Criteria

1. WHEN 编译追踪启用 THEN 追踪输出应该包含位置信息
2. WHEN 字节码被反汇编 THEN 输出应该显示每条指令对应的源代码行号
3. WHEN 调试器连接 THEN 调试器应该能够访问完整的行号表信息
4. WHEN 性能分析工具运行 THEN 工具应该能够将性能数据映射到源代码位置
5. WHEN 行号表数据被序列化 THEN 格式应该支持外部工具的读取和分析

### Requirement 6

**User Story:** 作为一个系统架构师，我希望行号表功能的实现能够保持良好的性能特性，不会显著影响编译和运行时性能。

#### Acceptance Criteria

1. WHEN 行号表被构建 THEN 构建过程应该是增量的，不需要额外的AST遍历
2. WHEN 行号表被查询 THEN 查询操作应该在O(log n)时间复杂度内完成
3. WHEN 行号表存储在内存中 THEN 内存占用应该与源代码大小成线性关系
4. WHEN 编译大型程序 THEN 行号表功能不应该显著增加编译时间
5. WHEN 虚拟机执行程序 THEN 行号表的存在不应该影响正常执行性能