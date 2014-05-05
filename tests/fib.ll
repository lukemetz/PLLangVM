
define %value @fib(%value* %env, %value %x){
    %1 = call %value @wrap_i32(i32 2)
    %2 = call %value @slt(%value %x, %value %1)
    %to_i8_1 = call i1 @extract_i1( %value %2)
    br i1 %to_i8_1, label %then1, label %else1
then1:
    %3 = call %value @wrap_i32(i32 1)
    br label %ifcont1
else1:
    %4 = call %value @wrap_i32(i32 1)
    %5 = call %value @sub(%value %x, %value %4)
    %6 =  call %value @fib (%value* null, %value %5)
    %7 = call %value @wrap_i32(i32 2)
    %8 = call %value @sub(%value %x, %value %7)
    %9 =  call %value @fib (%value* null, %value %8)
    %10 = call %value @add(%value %6, %value %9)
    br label %ifcont1
ifcont1:
    %11 =  phi %value [%3, %then1], [%10, %else1]
    ret %value %11
}


define void @main(%value* %env, %value %y){
    ret void
}


;BOILERPLATE code / initial functions
;0 --> none
; 1 is int
; 2 is func
; 3 is bool

%value = type {i8, i32*}
%func_t = type {%value (%value*, %value) *, %value *}

define i32* @malloc_i32() {
    %a_ptr_i8 = call noalias i8* @malloc(i64 4) #2
    %a_ptr = bitcast i8* %a_ptr_i8 to i32*
    ret i32* %a_ptr
}

define %value @wrap_i32(i32 %a) {
    %a_ptr = call i32* @malloc_i32()
    store i32 %a, i32* %a_ptr
    %a_tempstruct = insertvalue %value undef, i32* %a_ptr, 1
    %a_wrap = insertvalue %value %a_tempstruct, i8 1, 0
    ret %value %a_wrap
}

define %value @wrap_i1(i1 %a) alwaysinline {
    %a_ptr = call i32* @malloc_i32()
    %a_ptr_i1 = bitcast i32* %a_ptr to i1*
    store i1 %a, i1* %a_ptr_i1
    %a_tempstruct = insertvalue %value undef, i32* %a_ptr, 1
    %a_wrap = insertvalue %value %a_tempstruct, i8 3, 0
    ret %value %a_wrap
}

define %value* @malloc_env(i64 %size) alwaysinline {
    %array_size = mul i64 %size, 16 ;what? Probably the wrong size, but whose counting.
    %a_ptr_i8 = call noalias i8* @malloc(i64 %array_size)
    %a_ptr = bitcast i8* %a_ptr_i8 to %value*
    ret %value* %a_ptr
}

define %value @wrap_func(%value (%value*, %value) * %a, %value* %env) {
    %func_type_i8 = call noalias i8* @malloc(i64 16)
    %func_type_ptr = bitcast i8* %func_type_i8 to %func_t*
    %temp_func = insertvalue %func_t undef, %value (%value*, %value) * %a, 0
    %stack_func_t = insertvalue %func_t %temp_func, %value* %env, 1

    store %func_t %stack_func_t, %func_t* %func_type_ptr


    %a_ptr = bitcast %func_t* %func_type_ptr to i32 *
    %a_tempstruct = insertvalue %value undef, i32* %a_ptr, 1
    %a_wrap = insertvalue %value %a_tempstruct, i8 2, 0 ;The function type
    ret %value %a_wrap
}

define %value(%value*, %value)* @extract_func(%value %a) {
    %a_ptr = extractvalue %value %a, 1
    %a_func_t_ptr = bitcast i32* %a_ptr to %func_t*
    %a_func_t = load %func_t* %a_func_t_ptr
    %a_func_ptr = extractvalue %func_t %a_func_t, 0
    ret %value(%value*, %value)* %a_func_ptr
}


define %value* @extract_env(%value %a) {
    %a_ptr = extractvalue %value %a, 1
    %a_func_t_ptr = bitcast i32* %a_ptr to %func_t*
    %a_func_t = load %func_t* %a_func_t_ptr
    %a_func_ptr = extractvalue %func_t %a_func_t, 1
    ret %value* %a_func_ptr
}


define i1 @extract_i1(%value %a) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_ptr_i1 = bitcast i32* %a_ptr to i1*
    %a_val = load i1* %a_ptr_i1
    ret i1 %a_val
}

define %value @add(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = add i32 %a_val, %b_val
    %c_wrap = call %value @wrap_i32( i32 %c )
    ret %value %c_wrap
}

define %value @sub(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = sub i32 %a_val, %b_val
    %c_wrap = call %value @wrap_i32( i32 %c )
    ret %value %c_wrap
}

define %value @mul(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = mul i32 %a_val, %b_val
    %c_wrap = call %value @wrap_i32( i32 %c )
    ret %value %c_wrap
}

define %value @eq(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = icmp eq i32 %a_val, %b_val

    %c_wrap = call %value @wrap_i1( i1 %c )
    ret %value %c_wrap
}


define %value @slt(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = icmp slt i32 %a_val, %b_val
    %c_wrap = call %value @wrap_i1( i1 %c )
    ret %value %c_wrap
}


define %value @sgt(%value %a, %value %b) alwaysinline {
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr
    %b_ptr = extractvalue %value %b, 1
    %b_val = load i32* %b_ptr
    %c = icmp sgt i32 %a_val, %b_val
    %c_wrap = call %value @wrap_i1( i1 %c )
    ret %value %c_wrap
}

;Compiled by PLLangVM

@printFmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
define %value @print(%value* %env, %value %a) {
  entry:
    %a_ptr = extractvalue %value %a, 1
    %a_val = load i32* %a_ptr

    %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @printFmt, i32 0, i32 0), i32 %a_val)

    %ret_v = insertvalue %value undef, i8 0, 0
    ret %value %ret_v
}

declare noalias i8* @malloc(i64) #1
declare i32 @printf(i8*, ...)


; HELPERS
define void @printInt(i32 %i) {
    %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @printFmt, i32 0, i32 0), i32 %i)
    ret void
}
