package scad_clj.library;

import org.opencv.core.Core;

public class LibraryLoader {
    static {
	try {
	    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	} catch (UnsatisfiedLinkError e1) {
	    try {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME + "_x64");
	    } catch (UnsatisfiedLinkError er) {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME + "_mac");
	    }
	}
    }
	
    public static void force() {}
}
