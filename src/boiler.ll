;BOILERPLATE code / initial functions
;0 --> none
; 1 is int
; 2 is func
; 3 is bool

%value = type {i8, i32*}
%environment = type {i32, %value, %environment * }

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

define %value @wrap_func(%value (%value*, %value) * %a) {
    %a_ptr = bitcast %value(%value*,%value)* %a to i32 *
    %a_tempstruct = insertvalue %value undef, i32* %a_ptr, 1
    %a_wrap = insertvalue %value %a_tempstruct, i8 2, 0 ;The function type
    ret %value %a_wrap
}

define %value(%value*, %value)* @extract_func(%value %a) {
    %a_ptr = extractvalue %value %a, 1
    %a_func_ptr = bitcast i32* %a_ptr to %value(%value*, %value)*
    ret %value(%value*, %value)* %a_func_ptr
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
