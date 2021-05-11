; ModuleID = 'main'
source_filename = "main"

declare double @printfFloat(i8*, ...)

declare i32 @printfInt(i8*, ...)

define internal void @main() {
entry:
  call void @func1()
  %g = alloca i64, addrspace(4)
  %0 = load i64, i64 addrspace(4)* %g
  %1 = call i64 @func2(i64 10)
  %2 = call i64 @func2(i64 10)
  store i64 %2, i64 addrspace(4)* %g
  %3 = load i64, i64 addrspace(4)* %g
  call void @printI(i64 %3)
  ret void
}

declare void @printF(double)

declare void @printI(i64)

define internal void @func1() {
entry:
  %b = alloca double, addrspace(4)
  %0 = load double, double addrspace(4)* %b
  store double 2.300000e+00, double addrspace(4)* %b
  %c = alloca double, addrspace(4)
  %1 = load double, double addrspace(4)* %c
  store double 1.400000e+00, double addrspace(4)* %c
  %a = alloca double, addrspace(4)
  %arr = alloca [3 x double], addrspace(12)
  %2 = getelementptr [3 x double], [3 x double] addrspace(12)* %arr, i32 0, i32 0
  %3 = trunc i64 0 to i32
  %4 = getelementptr double, double addrspace(12)* %2, i32 %3
  store double 2.200000e+00, double addrspace(12)* %4
  %5 = getelementptr [3 x double], [3 x double] addrspace(12)* %arr, i32 0, i32 0
  %6 = trunc i64 1 to i32
  %7 = getelementptr double, double addrspace(12)* %5, i32 %6
  store double 3.300000e+00, double addrspace(12)* %7
  %8 = getelementptr [3 x double], [3 x double] addrspace(12)* %arr, i32 0, i32 0
  %9 = trunc i64 2 to i32
  %10 = getelementptr double, double addrspace(12)* %8, i32 %9
  store double 4.400000e+00, double addrspace(12)* %10
  %11 = getelementptr [3 x double], [3 x double] addrspace(12)* %arr, i32 0, i32 0
  %12 = trunc i64 0 to i32
  %13 = getelementptr double, double addrspace(12)* %11, i32 %12
  %14 = load double, double addrspace(12)* %13
  call void @printF(double %14)
  %15 = load double, double addrspace(4)* %a
  %16 = load double, double addrspace(4)* %b
  %17 = load double, double addrspace(4)* %c
  %18 = fadd double %16, %17
  %19 = load double, double addrspace(4)* %b
  %20 = load double, double addrspace(4)* %c
  %21 = fadd double %19, %20
  store double %21, double addrspace(4)* %a
  %22 = load double, double addrspace(4)* %a
  call void @printF(double %22)
  %23 = load double, double addrspace(4)* %a
  %24 = load double, double addrspace(4)* %b
  %25 = load double, double addrspace(4)* %c
  %26 = fsub double %24, %25
  %27 = load double, double addrspace(4)* %b
  %28 = load double, double addrspace(4)* %c
  %29 = fsub double %27, %28
  store double %29, double addrspace(4)* %a
  %30 = load double, double addrspace(4)* %a
  call void @printF(double %30)
  %31 = load double, double addrspace(4)* %a
  %32 = load double, double addrspace(4)* %b
  %33 = load double, double addrspace(4)* %c
  %34 = fmul double %32, %33
  %35 = load double, double addrspace(4)* %b
  %36 = load double, double addrspace(4)* %c
  %37 = fmul double %35, %36
  store double %37, double addrspace(4)* %a
  %38 = load double, double addrspace(4)* %a
  call void @printF(double %38)
  %39 = load double, double addrspace(4)* %a
  %40 = load double, double addrspace(4)* %b
  %41 = load double, double addrspace(4)* %c
  %42 = fdiv double %40, %41
  %43 = load double, double addrspace(4)* %b
  %44 = load double, double addrspace(4)* %c
  %45 = fdiv double %43, %44
  store double %45, double addrspace(4)* %a
  %46 = load double, double addrspace(4)* %a
  call void @printF(double %46)
  ret void
}

define internal i64 @func2(i64 %b1) {
entry:
  %b = alloca i64, addrspace(4)
  store i64 %b1, i64 addrspace(4)* %b
  %c = alloca i64, addrspace(4)
  %0 = load i64, i64 addrspace(4)* %c
  store i64 2, i64 addrspace(4)* %c
  %1 = load i64, i64 addrspace(4)* %b
  %2 = load i64, i64 addrspace(4)* %c
  %3 = add i64 %1, %2
  ret i64 %3
  ret void
}
