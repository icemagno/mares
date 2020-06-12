package testdll;

import com.sun.jna.Native;

public class Main {

	private static char[] toCharArray( String str ) {
		return str.toCharArray();		
	}
	
	public static void main(String[] args) {
		System.out.println("Hello");
		
		
		Previsao previsao = (Previsao) Native.loadLibrary("rotinas_mare_previsao", Previsao.class);
		
		char[] arq_const = toCharArray("C:\\Magno\\anhar\\constantes.txt");
		char[] arq_prev = toCharArray("C:\\Magno\\anhar\\DOJAVA.tmp");
		char[] tipo = toCharArray("2");
		char[] di = toCharArray("01");
		char[] mi = toCharArray("01");
		char[] ai = toCharArray("2018");
		char[] df = toCharArray("31");
		char[] mf = toCharArray("12");
		char[] af = toCharArray("2018");
		char[] nivel = toCharArray("0000068.87");
		char[] op = toCharArray("N");
		char[] sim = toCharArray("S");
		
		previsao.PREVISAO_MAXMIN_EXCEL(arq_const, arq_prev, tipo, di, mi, ai, df, mf, af, nivel, op, sim);
		
		
		

	}

}
