


define %value @func_1_5(%value* %env, %value %z){
    %localenv_extract_array = bitcast %value* %env to [2x %value]*
    %localenv_extract = load [2x %value]* %localenv_extract_array
    %y = extractvalue [2x %value] %localenv_extract, 1
    %x = extractvalue [2x %value] %localenv_extract, 0
    %1 = call %value @add(%value %x, %value %y)
    %2 = call %value @mul(%value %1, %value %z)
    ret %value %2
}

define %value @func_1_3(%value* %env, %value %y){
    %localenv_extract_array = bitcast %value* %env to [1x %value]*
    %localenv_extract = load [1x %value]* %localenv_extract_array
    %x = extractvalue [1x %value] %localenv_extract, 0
    %ar_1_0 = insertvalue [ 2 x %value] undef, %value %x, 0
    %ar_1_1 = insertvalue [ 2 x %value] %ar_1_0, %value %y, 1
    %localenv_1 = call %value* @malloc_env(i64 2)
    %localenv_1_array = bitcast %value* %localenv_1 to [ 2 x %value]*
    store [ 2 x %value] %ar_1_1, [ 2 x %value]* %localenv_1_array
    %localenv_1_ptr = bitcast [ 2 x %value]* %localenv_1_array to %value*
    %1 = call %value @wrap_func(%value(%value*, %value)* @func_1_5, %value* %localenv_1_ptr)
    ret %value %1
}

define %value @test(%value* %env, %value %x){
    %ar_1_0 = insertvalue [ 1 x %value] undef, %value %x, 0
    %localenv_1 = call %value* @malloc_env(i64 1)
    %localenv_1_array = bitcast %value* %localenv_1 to [ 1 x %value]*
    store [ 1 x %value] %ar_1_0, [ 1 x %value]* %localenv_1_array
    %localenv_1_ptr = bitcast [ 1 x %value]* %localenv_1_array to %value*
    %1 = call %value @wrap_func(%value(%value*, %value)* @func_1_3, %value* %localenv_1_ptr)
    ret %value %1
}


define void @main(%value* %env, %value %x){
    %1 = call %value @wrap_i32(i32 1)
    %2 = call %value @wrap_i32(i32 1)
    %3 =  call %value @test (%value* null, %value %2)
    %4 = call %value @wrap_i32(i32 2)
    %func_ptr_5 = call %value(%value*, %value)*(%value)* @extract_func(%value %3)
    %func_env_5 = call %value* @extract_env(%value %3)
    %5 =  call %value %func_ptr_5(%value * %func_env_5, %value %4)
    %6 = call %value @wrap_i32(i32 3)
    %func_ptr_7 = call %value(%value*, %value)*(%value)* @extract_func(%value %5)
    %func_env_7 = call %value* @extract_env(%value %5)
    %7 =  call %value %func_ptr_7(%value * %func_env_7, %value %6)
    %8 = call %value @wrap_i32(i32 1)
    %9 = call %value @wrap_i32(i32 1)
    %10 =  call %value @test (%value* null, %value %9)
    %11 = call %value @wrap_i32(i32 2)
    %func_ptr_12 = call %value(%value*, %value)*(%value)* @extract_func(%value %10)
    %func_env_12 = call %value* @extract_env(%value %10)
    %12 =  call %value %func_ptr_12(%value * %func_env_12, %value %11)
    %13 = call %value @wrap_i32(i32 3)
    %func_ptr_14 = call %value(%value*, %value)*(%value)* @extract_func(%value %12)
    %func_env_14 = call %value* @extract_env(%value %12)
    %14 =  call %value %func_ptr_14(%value * %func_env_14, %value %13)
    %15 =  call %value @print (%value* null, %value %14)
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
