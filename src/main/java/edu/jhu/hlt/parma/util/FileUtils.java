// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util;

import java.io.*;
import java.util.zip.*;

public class FileUtils {
	
	public static final String DEFAULT_ENCODING = "UTF-8";

	public static BufferedReader getReader(String filename) { return getReader(filename, "UTF-8"); }
	public static BufferedReader getReader(String filename, String encoding) { return getReader(new File(filename), encoding); }
	public static BufferedReader getReader(File input) { return getReader(input, DEFAULT_ENCODING); }
	public static BufferedReader getReader(File input, String encoding) {
		try {
			InputStream is = new FileInputStream(input);
			if(input.getName().endsWith(".gz"))
				is = new GZIPInputStream(is);
			return new BufferedReader(new InputStreamReader(is, encoding));
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static BufferedWriter getWriter(String filename) { return getWriter(new File(filename), DEFAULT_ENCODING); }
	public static BufferedWriter getWriter(String filename, String encoding) { return getWriter(new File(filename), encoding); }
	public static BufferedWriter getWriter(File file) { return getWriter(file, DEFAULT_ENCODING); }
	public static BufferedWriter getWriter(File file, String encoding) {
		try {
			OutputStream os = new FileOutputStream(file);
			if(file.getName().endsWith(".gz"))
				os = new GZIPOutputStream(os);
			return new BufferedWriter(new OutputStreamWriter(os, encoding));
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static Object deserialize(String fname) { return deserialize(new File(fname), false); }
	public static Object deserialize(String fname, boolean gzipped) { return deserialize(new File(fname), gzipped); }
	public static Object deserialize(File f) { return deserialize(f, false); }
	public static Object deserialize(File f, boolean gzipped) {
		try {
			InputStream is = new FileInputStream(f);
			if(gzipped) is = new GZIPInputStream(is);
			ObjectInputStream ois = new ObjectInputStream(is);
			Object obj = ois.readObject();
			ois.close();
			return obj;
		} catch(Exception e) { throw new RuntimeException(e); }
	}

	public static void serialize(Object obj, String fname, boolean gzipped) { serialize(obj, new File(fname), gzipped); }
	public static void serialize(Object obj, String fname) { serialize(obj, new File(fname), false); }
	public static void serialize(Object obj, File f) { serialize(obj, f, false); }
	public static void serialize(Object obj, File f, boolean gzipped) {
		try {
			OutputStream os = new FileOutputStream(f);
			if(gzipped) os = new GZIPOutputStream(os);
			ObjectOutputStream oos = new ObjectOutputStream(os);
			oos.writeObject(obj);
			oos.close();
		} catch(Exception e) { throw new RuntimeException(e); }
	}

}

