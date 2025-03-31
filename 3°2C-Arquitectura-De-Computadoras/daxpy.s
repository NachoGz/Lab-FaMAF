	.data
	N:       .dword 4096	// Number of elements in the vectors
	Alpha:   .dword 2      // scalar value

	.bss
	X: .zero  32768        // vector X(4096)*8
	Y: .zero  32768        // Vector Y(4096)*8
    Z: .zero  32768        // Vector Z(4096)*8

	.arch armv8-a
	.text
	.align	2
	.global	main
	.type	main, %function
main:
.LFB6:
	.cfi_startproc
	stp	x29, x30, [sp, -16]!
	.cfi_def_cfa_offset 16
	.cfi_offset 29, -16
	.cfi_offset 30, -8
	mov	x29, sp
	mov	x1, 0
	mov	x0, 0
	bl	m5_dump_stats

	ldr     x0, N
    	ldr     x10, =Alpha
    	ldr     x2, =X
    	ldr     x3, =Y
	ldr     x4, =Z

//---------------------- CODE HERE ------------------------------------
	add     x5, x0, 0          // i = N
	mov     x7, xzr            // j = 0 (comenzar desde el principio)

	ldr     d0, [x10]          // Cargar alpha
.loop:
	// Cargar X[j] y X[i] consecutivamente
	lsl     x8, x7, 3          // j * 8
	ldr     d4, [x2, x8]       // Cargar X[j]
	add     x7, x7, 1          // j++
	lsl     x8, x7, 3          // (j+1) * 8
	ldr     d1, [x2, x8]       // Cargar X[j+1]
	
	// Cargar Y[j] y Y[j+1] consecutivamente
	sub     x8, x8, 8          // volver a j * 8
	ldr     d5, [x3, x8]       // Cargar Y[j]
	add     x8, x8, 8          // (j+1) * 8
	ldr     d2, [x3, x8]       // Cargar Y[j+1]

	// Calcular resultados
	fmul    d6, d0, d4         // alpha * X[j]
	fmul    d3, d0, d1         // alpha * X[j+1]
	
	fadd    d6, d6, d5         // alpha * X[j] + Y[j]
	fadd    d3, d3, d2         // alpha * X[j+1] + Y[j+1]

	// Almacenar resultados consecutivamente
	sub     x8, x8, 8          // volver a j * 8
	str     d6, [x4, x8]       // Guardar Z[j]
	add     x8, x8, 8          // (j+1) * 8
	str     d3, [x4, x8]       // Guardar Z[j+1]

	sub     x5, x5, 2          // Decrementar contador por 2
	add     x7, x7, 1          // j += 1 (incremento total de 2 por iteración)
	
	cbnz    x5, .loop          // Continuar si no hemos terminado

.end_loop:

//---------------------- END CODE ------------------------------------

	mov 	x0, 0
	mov 	x1, 0
	bl	m5_dump_stats
	mov	w0, 0
	ldp	x29, x30, [sp], 16
	.cfi_restore 30
	.cfi_restore 29
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE6:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
