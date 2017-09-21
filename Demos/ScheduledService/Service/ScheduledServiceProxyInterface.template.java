//*******************************************************
//
//           CodeGear Delphi Runtime Library
// Copyright(c) 2014-2017 Embarcadero Technologies, Inc.
//              All rights reserved
//
//*******************************************************

package com.embarcadero.services;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

    public class <%ServiceName%>ProxyInterface implements InvocationHandler{
        long pointer;

        public Object CreateProxyClass(Class listenerClass, long pointer) throws ClassNotFoundException{
              this.pointer = pointer;
              return Proxy.newProxyInstance(listenerClass.getClassLoader(), new Class[]{listenerClass}, this);
        }

        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            Object Obj = dispatchToNative(method.getName(), args, pointer);
            cleanNative(pointer);
    	    return Obj;
        }

        public native Object dispatchToNative(String method, Object[] args, long pointer);
        public native void cleanNative(long pointer);
    }
