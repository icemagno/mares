package mb.casnav.chm.controller;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import mb.casnav.chm.mappers.AlturaMapper;
import mb.casnav.chm.mappers.EstacaoApiMapper;
import mb.casnav.chm.mappers.LuaMapper;
import mb.casnav.chm.mappers.TabuaMaresMapper;
import mb.casnav.chm.model.Altura;
import mb.casnav.chm.model.EstacaoAPI;
import mb.casnav.chm.model.Lua;
import mb.casnav.chm.model.Previsao;
import mb.casnav.chm.model.Previsoes;
import mb.casnav.chm.model.TabuaMares;

@RestController
@RequestMapping("/v1")
public class V1Controller {

	@Autowired
    JdbcTemplate jdbcTemplate;	
	
	@Value("${anhar.location}")
	private String anharLocation;
	
	@Value("${anhar.outputFolder}")
	private String outputFolder;
	
	@RequestMapping(value = "/estacoes/{tabua}", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<EstacaoAPI> getEstacoes( @PathVariable("tabua") String ehTabua ) {
		Object[] params = new Object[] { ehTabua };
		String sql = "select * from dbo.vw_sisgeodef_estacoes_maregraficas where tabua = ?";
		List<EstacaoAPI> estacoes = jdbcTemplate.query(sql, params, new EstacaoApiMapper() );
		return estacoes;
	}

	
	@RequestMapping(value = "/estacoes", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<EstacaoAPI> getEstacoes(  ) {
		Object[] params = new Object[] { };
		String sql = "select * from dbo.vw_sisgeodef_estacoes_maregraficas";
		List<EstacaoAPI> estacoes = jdbcTemplate.query(sql, params, new EstacaoApiMapper() );
		return estacoes;
	}

	
	@RequestMapping(value = "/tabuas", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<TabuaMares> getTabuas(  ) {
		Object[] params = new Object[] { };
		String sql = "select * from dbo.vw_sisgeodef_tabua_mares";
		List<TabuaMares> tabuas = jdbcTemplate.query(sql, params, new TabuaMaresMapper() );
		return tabuas;
	}	

	@RequestMapping(value = "/tabua/{estacao}", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<TabuaMares> getTabua( @PathVariable("estacao") Integer codEstacao ) {
		Object[] params = new Object[] { codEstacao };
		String sql = "select * from dbo.vw_sisgeodef_tabua_mares where cod_estacao_maregrafica = ?";
		List<TabuaMares> tabuas = jdbcTemplate.query(sql, params, new TabuaMaresMapper() );
		return tabuas;
	}	
	
	@RequestMapping(value = "/alturas/{tabua}", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<Altura> getAlturas( @PathVariable("tabua") Integer codTabua ) {
		Object[] params = new Object[] { codTabua };
		String sql = "select * from dbo.vw_sisgeodef_tabua_mares_alturas where cod_tabua = ? order by data_hora asc";
		List<Altura> alturas = jdbcTemplate.query(sql, params, new AlturaMapper() );
		return alturas;
	}	
	

	@RequestMapping(value = "/alturas", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<Altura> getAlturas( ) {
		Object[] params = new Object[] { };
		String sql = "select * from dbo.vw_sisgeodef_tabua_mares_alturas order by data_hora asc";
		List<Altura> alturas = jdbcTemplate.query(sql, params, new AlturaMapper() );
		return alturas;
	}	

	@RequestMapping(value = "/luas/{ano}", method = RequestMethod.GET, produces = "application/json;charset=UTF-8")
	public List<Lua> getLuas( @PathVariable("ano") Integer ano ) {
		Object[] params = new Object[] { ano };
		String sql = "select * from dbo.fases_lua_tu where datepart( year, data_hora) = ?";
		
		List<Lua> luas = jdbcTemplate.query(sql, params, new LuaMapper() );
		return luas;
	}	

	
	@RequestMapping(value = "/anhar/{estacao}/{ano}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_UTF8_VALUE )
	public Previsoes runAnhar( @PathVariable("estacao") String estacao, @PathVariable("ano") String ano ) {
		
		// 256PREVHORCOL.txt
		// 256PREVMAXMINCOL.txt
		// constantes_256.txt
		
		Previsoes prevs = new Previsoes();
		File prevHor = new File( outputFolder + estacao + "PREVHORCOL.txt" );
		File prevMaxMin = new File( outputFolder + estacao + "PREVMAXMINCOL.txt" );
		
		if( !prevHor.exists() ) {
			try {
				Process process = new ProcessBuilder( anharLocation ,"6",estacao,ano, outputFolder).start();
				process.waitFor();
			} catch ( Exception e ) {
				e.printStackTrace();
			}
		}
		
		if( prevHor.exists() ) {
		
			try {
				FileReader fr = new FileReader( prevHor );
				BufferedReader br = new BufferedReader(fr);			
				String sCurrentLine;
				while ((sCurrentLine = br.readLine()) != null) {
					
					if( sCurrentLine != null && !sCurrentLine.trim().equals("") ) {
						String data = sCurrentLine.substring(0, 10);
						String hora = sCurrentLine.substring(10, 16);
						String valor = sCurrentLine.substring(16, sCurrentLine.length() );
						prevs.addHoraria( new Previsao(data.trim(), hora.trim(), valor.trim() ) );
					}
					
				}			
				if (br != null)	br.close();
				if (fr != null)	fr.close();
			} catch( Exception ee ) {
				ee.printStackTrace();
			}
			
		}

		if( prevMaxMin.exists() ) {
			
			try {
				FileReader fr = new FileReader( prevMaxMin );
				BufferedReader br = new BufferedReader(fr);			
				String sCurrentLine;
				while ((sCurrentLine = br.readLine()) != null) {
					if( sCurrentLine != null && !sCurrentLine.trim().equals("") ) {

						String data = sCurrentLine.substring(0, 10);
						String hora = sCurrentLine.substring(10, 16);
						String valor = sCurrentLine.substring(16, sCurrentLine.length() );
						prevs.addMaxMin( new Previsao(data.trim(), hora.trim(), valor.trim() ) );
					
					}
				}			
				if (br != null)	br.close();
				if (fr != null)	fr.close();
			} catch( Exception ee ) {
				ee.printStackTrace();
			}
			
		}
		
		return prevs;
		
	}	
	
	
	
	
}
