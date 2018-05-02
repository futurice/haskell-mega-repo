// Copied from Python.h
typedef struct PyObject PyObject;
// int PyArg_ParseTuple(PyObject *args, const char *format, ...);
PyObject* PyObject_GetAttrString(PyObject *o, const char *attr_name);
char* PyString_AsString (PyObject *string);

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
