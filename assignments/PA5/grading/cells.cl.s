# start of generated code
	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
_int_tag:
	.word	2
_bool_tag:
	.word	3
_string_tag:
	.word	4
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
str_const16:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const1
	.byte	0	
	.align	2
	.word	-1
str_const15:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const14:
	.word	4
	.word	9
	.word	String_dispTab
	.word	int_const4
	.ascii	"CellularAutomaton"
	.byte	0	
	.align	2
	.word	-1
str_const13:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const5
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const12:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const6
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const5
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const8
	.ascii	"_prim_slot"
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const9
	.ascii	"SELF_TYPE"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const9
	.ascii	"_no_class"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	4
	.word	8
	.word	String_dispTab
	.word	int_const10
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	4
	.word	9
	.word	String_dispTab
	.word	int_const11
	.ascii	"         X         "
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.ascii	"."
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.ascii	"X"
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.ascii	"\n"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const8
	.ascii	"./cells.cl"
	.byte	0	
	.align	2
	.word	-1
int_const11:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	19
	.word	-1
int_const10:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const9:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	9
	.word	-1
int_const8:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	10
	.word	-1
int_const7:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const6:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const5:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const4:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	17
	.word	-1
int_const3:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const2:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	20
	.word	-1
int_const1:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
int_const0:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	1
	.word	-1
bool_const0:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:
	.word	str_const9
	.word	str_const10
	.word	str_const14
	.word	str_const11
	.word	str_const12
	.word	str_const13
	.word	str_const15
class_objTab:
	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	CellularAutomaton_protObj
	.word	CellularAutomaton_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
	.word	Main_protObj
	.word	Main_init
Object_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
IO_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
CellularAutomaton_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
	.word	CellularAutomaton.init
	.word	CellularAutomaton.print
	.word	CellularAutomaton.num_cells
	.word	CellularAutomaton.cell
	.word	CellularAutomaton.cell_left_neighbor
	.word	CellularAutomaton.cell_right_neighbor
	.word	CellularAutomaton.cell_at_next_evolution
	.word	CellularAutomaton.evolve
Int_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
Bool_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
String_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	String.length
	.word	String.concat
	.word	String.substr
Main_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	Main.main
	.word	-1
Object_protObj:
	.word	0
	.word	3
	.word	Object_dispTab
	.word	-1
IO_protObj:
	.word	1
	.word	3
	.word	IO_dispTab
	.word	-1
CellularAutomaton_protObj:
	.word	2
	.word	4
	.word	CellularAutomaton_dispTab
	.word	0
	.word	-1
Int_protObj:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	int_const1
	.word	-1
Bool_protObj:
	.word	4
	.word	4
	.word	Bool_dispTab
	.word	-1
String_protObj:
	.word	5
	.word	5
	.word	String_dispTab
	.word	str_const16
	.word	-1
Main_protObj:
	.word	6
	.word	4
	.word	Main_dispTab
	.word	0
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
Object_init:
# this is where class init method goes
IO_init:
# this is where class init method goes
CellularAutomaton_init:
# this is where class init method goes
Int_init:
# this is where class init method goes
Bool_init:
# this is where class init method goes
String_init:
# this is where class init method goes
Main_init:
# this is where class init method goes
CellularAutomaton.init:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	lw	$ra 4($sp)
	addiu	$sp $sp 8
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.print:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	la	$a0 str_const1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal String.concat
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.out_string
	lw	$ra 4($sp)
	addiu	$sp $sp 4
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.num_cells:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	jal String.length
	lw	$ra 4($sp)
	addiu	$sp $sp 4
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.cell:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal String.substr
	lw	$ra 4($sp)
	addiu	$sp $sp 8
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.cell_left_neighbor:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const1
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label0
	la	$a0 bool_const0
label0:
	la	$t1 bool_const1
	beq	$a0 $t1 label1
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	sub	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell
label1:
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.num_cells
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	sub	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell
	lw	$ra 4($sp)
	addiu	$sp $sp 8
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.cell_right_neighbor:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.num_cells
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	sub	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label2
	la	$a0 bool_const0
label2:
	la	$t1 bool_const1
	beq	$a0 $t1 label3
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	add	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell
label3:
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell
	lw	$ra 4($sp)
	addiu	$sp $sp 8
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.cell_at_next_evolution:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 str_const2
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label4
	la	$a0 bool_const0
label4:
	la	$t1 bool_const1
	beq	$a0 $t1 label5
	la	$a0 int_const1
label5:
	la	$a0 int_const0
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell_left_neighbor
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 str_const2
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label6
	la	$a0 bool_const0
label6:
	la	$t1 bool_const1
	beq	$a0 $t1 label7
	la	$a0 int_const1
label7:
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	add	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	jal SELF_TYPE.cell_right_neighbor
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 str_const2
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label8
	la	$a0 bool_const0
label8:
	la	$t1 bool_const1
	beq	$a0 $t1 label9
	la	$a0 int_const1
label9:
	la	$a0 int_const0
	jal	Object.copy
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	add	$t1 $t1 $t2
	sw	$t1 12($a0)
	addiu	$sp $sp 4
	lw	$a0 12($a0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 int_const0
	lw	$t1 12($a0)
	lw	$t2 16($sp)
	addiu	$sp $sp 4
	la	$a0 bool_const1
	beq	$t1 $t2 label10
	la	$a0 bool_const0
label10:
	la	$t1 bool_const1
	beq	$a0 $t1 label11
	la	$a0 str_const3
label11:
	la	$a0 str_const2
	lw	$ra 4($sp)
	addiu	$sp $sp 8
	lw	$fp 0($sp)
	jalr		$ra
CellularAutomaton.evolve:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	lw	$ra 4($sp)
	addiu	$sp $sp 4
	lw	$fp 0($sp)
	jalr		$ra
Main.main:
	move	$fp $sp
	sw	$ra 0($sp)
	addiu	$sp $sp -4
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	jal CellularAutomaton.print
	lw	$ra 4($sp)
	addiu	$sp $sp 4
	lw	$fp 0($sp)
	jalr		$ra

# end of generated code
