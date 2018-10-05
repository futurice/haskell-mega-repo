#include <stddef.h>

// Copied from Python.h
typedef struct PyObject PyObject;
// int PyArg_ParseTuple(PyObject *args, const char *format, ...);
PyObject* PyObject_GetAttrString(PyObject *o, const char *attr_name);
char* PyString_AsString (PyObject *string);
long PyInt_AsLong(PyObject *io);
PyObject* PyEval_CallObject(PyObject* x,PyObject* args);
void Py_DecRef(PyObject *o);
void PyErr_Clear();
PyObject* PyObject_CallMethod(PyObject *o, char *method, char *format, ...);

void PyErr_Print();

char *futuriceLambdaFunctionName(PyObject *context) {
    PyObject *attr = PyObject_GetAttrString(context, "function_name");
    if (!attr) {
        return "UnknownFunction";
    }

    char *name = PyString_AsString(attr);
    if (!name) {
        return "UnknownFunction";
    }

    return name;
}

long futuriceLambdaTimeRemainingInMillis(PyObject *context) {
    PyObject *res = PyObject_CallMethod(context, "get_remaining_time_in_millis", NULL);
    if (!res) {
        // PyErr_Print();
        return -1;
    }

    long n = PyInt_AsLong(res);
    Py_DecRef(res);
    PyErr_Clear();

    return n;
}
