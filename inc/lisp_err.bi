#ifndef __LISP_ERR_BI__
#define __LISP_ERR_BI__

/'
 * Copyright (c) 2007-2008 Jeffery R. Marshall.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'/

namespace LISP

	enum LISP_ERROR

		LISP_ERR_SUCCESS = 0
		LISP_ERR_FUNCTION_NOT_DEFINED
		LISP_ERR_INVALID_ARGUMENT
		LISP_ERR_SETTING_VALUE_OF_NIL_OBJECT
		LISP_ERR_CDR_OF_NON_CONS
		LISP_ERR_CAR_OF_NON_CONS
		LISP_ERR_UNEXPECTED_DOT
		LISP_ERR_UNEXPECTED_RIGHT_PARA
		LISP_ERR_UNEXPECTED_TOKEN
		LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS
		LISP_ERR_DIVISION_BY_ZERO
		LISP_ERR_ARGUMENT_TYPE_MISMATCH
		LISP_ERR_TOO_FEW_ARGUMENTS

		LISP_ERRS
		
	end enum

end namespace

#endif
