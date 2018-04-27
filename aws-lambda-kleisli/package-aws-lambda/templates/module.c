#include <Python.h>
#include <stdio.h>
#include "HsFFI.h"
#include "Rts.h"

{{#handlers}}
extern HsPtr {{hs}}(const char *event, HsPtr context, HsPtr logging);

static PyObject* {{hs}}Wrapper(PyObject *self, PyObject *args) {
	const char* event;
	PyObject* context;
	PyObject* logging;

	// https://docs.python.org/2.7/c-api/arg.html
	// first argument is a string, second is a context
	if (!PyArg_ParseTuple(args, "sOO:{{py}}", &event, &context, &logging)) {
		return NULL;
	}

	// Check that third argument is c allable
	if (!PyCallable_Check(logging)) {
		PyErr_SetString(PyExc_TypeError, "3rd parameter must be callable");
		return NULL;
	}

	// Run Haskell computation
	char *res = {{hs}}(event, context, logging);

	// When memory buffers are passed as parameters to supply data to build objects, as for the s and s# formats, the required data is copied.
	return Py_BuildValue("s", res);
}
{{/handlers}}

// http://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/ffi-chap.html#making-a-haskell-library-that-can-be-called-from-foreign-code
//
// (args? : [string]) : None
static PyObject *hs_init_wrapper(PyObject *self, PyObject *args) {
	// https://stackoverflow.com/a/42868330/1308058
	PyObject *pList = NULL;

	// parse arguments, optional list.
	if (!PyArg_ParseTuple(args, "|O!:hs_init", &PyList_Type, &pList)) {
		return NULL;
	}

	// no arguments: hs_init without arguments
	if (pList == NULL) {
		int argc = 0;
		char *argv[] = { NULL };
		char **pargv = argv;
		hs_init(&argc, &pargv);

		// do any other initialization here and
		Py_RETURN_NONE;
	} else {
		// with arguments, extract strings (char *) from Python list.
		Py_ssize_t n = PyList_Size(pList);

		int argc = n;
		char **argv = malloc(sizeof(char *) * (argc + 1));
		argv[argc] = NULL;

		for (int i = 0; i < argc; i++) {
			PyObject *pItem = PyList_GetItem(pList, i);
			if (!PyString_Check(pItem)) {
				free(argv);
				PyErr_SetString(PyExc_TypeError, "list items must be strings.");
				return NULL;
			}

			argv[i] = PyString_AsString(pItem);
		}

		RtsConfig conf = defaultRtsConfig;
		conf.rts_opts_enabled = RtsOptsAll;
		hs_init_ghc(&argc, &argv, conf);
		free(argv);

		Py_RETURN_NONE;
	}
}

static PyObject *hs_exit_wrapper(PyObject *self, PyObject *args) {
	hs_exit();
	Py_RETURN_NONE;
}

static PyMethodDef methods[] = {
{{#handlers}}
	{
		"{{py}}", {{hs}}Wrapper, METH_VARARGS,
		"AWS Lambda handler"
	},
{{/handlers}}
	{
		"hs_init", hs_init_wrapper, METH_VARARGS,
		"The call to hs_init() initializes GHC's runtime system. Do NOT try to invoke any Haskell functions before calling hs_init(): bad things will undoubtedly happen."
	},
	{
		"hs_exit", hs_exit_wrapper, METH_VARARGS,
		"Cleanup Haskell RTS"
	},
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC init{{nativeModuleName}}(void) {
	(void) Py_InitModule ("{{nativeModuleName}}", methods);
}
