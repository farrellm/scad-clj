package scad_clj.library;

import org.opencv.core.Core;

public class LibraryLoader {
    static {
	System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
    }
	
    public static void force() {}
}
