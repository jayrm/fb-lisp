#ifndef __LISP_DEBUG_BI__
#define __LISP_DEBUG_BI__

#ifdef DEBUG

	#macro DECLARE_DEBUG_ALLOCATOR()
		declare operator new( byval size as uinteger ) as any ptr
		declare operator new[]( byval size as uinteger ) as any ptr
		declare operator delete( byval buf as any ptr )
		declare operator delete[]( byval buf as any ptr )
	#endmacro

	#macro DEFINE_DEBUG_ALLOCATOR( t )
		operator t.new( byval size as uinteger ) as any ptr
			operator = lisp.allocate( size, __FILE__, __LINE__ )
		end operator

		operator t.new[]( byval size as uinteger ) as any ptr
			operator = lisp.allocate( size, __FILE__, __LINE__ )
		end operator

		operator t.delete( byval buf as any ptr )
			lisp.deallocate( buf, __FILE__, __LINE__  )
		end operator

		operator t.delete[]( byval buf as any ptr )
			lisp.deallocate( buf, __FILE__, __LINE__ )
		end operator
	#endmacro

#else

	#define DECLARE_DEBUG_ALLOCATOR()
	#define DEFINE_DEBUG_ALLOCATOR( t )

#endif

#endif
