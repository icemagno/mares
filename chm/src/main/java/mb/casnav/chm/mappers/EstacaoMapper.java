package mb.casnav.chm.mappers;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import mb.casnav.chm.model.Estacao;

public final class EstacaoMapper implements RowMapper<Estacao>{

	@Override
	public Estacao mapRow(ResultSet rs, int rowNum) throws SQLException {
		Estacao md = new Estacao();
		md.setNomeEstacaoMaregrafica( rs.getString("nome_estacao_maregrafica") );
		md.setCodEstacaoMaregrafica( rs.getString("cod_estacao_maregrafica") );
		md.setNumEstacaoMaregrafica( rs.getString("num_estacao_maregrafica") );
		md.setLatitude( rs.getString("latitude") );
		md.setLongitude( rs.getString("longitude") );
		md.setFuso( rs.getString("fuso") );
		md.setTabua( rs.getString("tabua") );
		md.setLocalOrigem( rs.getString("local_origem") );
		md.setInstituicao( rs.getString("instituicao") );
		md.setNumAna( rs.getString("num_ana") );
		md.setFluviometrica( rs.getString("fluviometrica") );
		return md;
	}

}
