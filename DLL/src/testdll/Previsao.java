package testdll;

import com.sun.jna.Library;

public interface Previsao extends Library {
	public void PREVISAO_MAXMIN_EXCEL( char[] arq_const, char[] arq_prev, char[] tipo, char[] di, char[] mi, char[] ai, char[] df, char[] mf, char[] af, char[] nivel, char[] op, char[] sim );

}
