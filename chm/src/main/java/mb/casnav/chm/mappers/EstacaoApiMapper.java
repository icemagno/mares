package mb.casnav.chm.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import mb.casnav.chm.model.EstacaoAPI;

public final class EstacaoApiMapper implements RowMapper<EstacaoAPI>{

	@Override
	public EstacaoAPI mapRow(ResultSet rs, int rowNum) throws SQLException {
		EstacaoAPI md = new EstacaoAPI();
		md.setNomeEstacaoMaregrafica( rs.getString("nome_estacao_maregrafica") );
		md.setCodEstacaoMaregrafica( rs.getString("cod_estacao_maregrafica") );
		md.setNumEstacaoMaregrafica( rs.getString("num_estacao_maregrafica") );
		md.setLatitude( rs.getString("latitude") );
		md.setLongitude( rs.getString("longitude") );
		md.setFuso( rs.getString("fuso") );
		md.setTabua( rs.getString("tabua") );
		md.setLocalOrigem( rs.getString("local_origem") );
		md.setInstituicao( rs.getString("instituicao") );
		md.setNomePais( rs.getString("nome_pais") );
		md.setNomePorto( rs.getString("nome_porto") );
		md.setCartaPorto( rs.getString("carta_porto") );
		md.setCodPorto( rs.getString("cod_porto") );
		
		md.setLatitudeConvertida( rs.getString("latitude_convertida") );
		md.setLongitudeConvertida( rs.getString("longitude_convertida") );
		
		
		return md;
	}

}
