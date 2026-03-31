	.data
buffer:				.byte '___________________________________'
in_port:    		.word 0x80
out_port:   		.word 0x84
right_ptr: 	   	   	.word 0
left_ptr: 			.word 0
saved_ptr: 			.word 0
temp1:		   		.word 0
temp2: 				.word 0
byte_mask:	   		.word 0x000000ff
mask_for_store: 	.word 0xffffff00
one_const:	       	.word 1
new_line: 	       	.word 10
count_overflow:		.word 32
counter:      		.word 0
length: 			.word 0

	.text
	.org 	0x88

_start:
	load_imm		buffer
	store	 		right_ptr	
	
read_input:	
	load			in_port
	load_acc
	and				byte_mask
	store			temp1
	beqz			save_ptr
continue:
	load			temp1
	xor				new_line
	beqz			put_null
	
	load	 		right_ptr
	load_acc
	and				mask_for_store
	add				temp1
	store_ind		right_ptr
	
	load			right_ptr
	add				one_const
	store			right_ptr
	
	load            counter
    add             one_const
    store           counter
	xor				count_overflow
	beqz			overflow_error
    
	jmp				read_input

save_ptr:
	load			right_ptr
	store			saved_ptr
	load 			counter
	store 			length
	jmp				continue
	
put_null:
	load			right_ptr
	load_acc
	and				mask_for_store		
	store_ind		right_ptr
	jmp 			check_saved_ptr

check_saved_ptr:
	load			saved_ptr
	beqz 			reverse_input
	store			right_ptr
	
reverse_input:
	load			right_ptr
	sub				one_const
	store			right_ptr
	
	load			length
	beqz			calculate_count_of_steps
	store			counter
	
calculate_count_of_steps:
	load			counter
	shiftr 			one_const			
	store			counter
	
	load_imm		buffer
	store			left_ptr
	
reverse_loop:	
	load			counter
	beqz			print_buffer
	sub				one_const
	store 			counter
	
	load			right_ptr
	load_acc
	and 			byte_mask
	store			temp1
	
	load			left_ptr
	load_acc
	and				byte_mask
	store			temp2
	
	load			left_ptr
    load_acc
	and             mask_for_store
	add				temp1
	store_ind		left_ptr
	
	load			right_ptr
	load_acc
	and				mask_for_store
	add             temp2
	store_ind		right_ptr
	
	load			right_ptr
	sub				one_const
	store			right_ptr
	
	load			left_ptr
	add				one_const
	store 			left_ptr

	jmp 			reverse_loop
	
	
print_buffer:
	load_imm		buffer
	store			right_ptr

while_result:
	load			right_ptr
	load_acc
	and 			byte_mask
	beqz			end
	store_ind		out_port
	
	load			right_ptr
	add				one_const
	store			right_ptr
	
	jmp 			while_result
	
	
overflow_error:
	load_imm		0xcccccccc
	store_ind		out_port	
	
end:
    halt