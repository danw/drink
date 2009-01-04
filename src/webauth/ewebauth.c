// -------------------------------------------------------------------
//  File    : ewebauth.c
//  Author  : Dan Willemsen <dan@csh.rit.edu>
//  Purpose : 
// 
// 
//  edrink, Copyright (C) 2008 Dan Willemsen
// 
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//                          
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
//  02111-1307 USA
// 
// -------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_interface.h>
#include <ei.h>
#include <unistd.h>
#include <webauth.h>
#include <assert.h>

#define dec_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#define enc_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#define BUFSIZE (1 << 16)

static int session_key_req(char *keytab, char *kdc_princ, char **k5_req, int *k5_req_len) {
    WEBAUTH_KRB5_CTXT *ctxt;
    int status;
    
    status = webauth_krb5_new(&ctxt);
    if(status != WA_ERR_NONE) {
        // TODO: log error
        fprintf(stderr, "Got error from krb5_new %d\n", status);
        if(status == WA_ERR_KRB5)
            webauth_krb5_free(ctxt);
        return 0;
    }
    
    status = webauth_krb5_init_via_keytab(ctxt, keytab, NULL, NULL);
    if(status != WA_ERR_NONE) {
        // TODO: log error
        fprintf(stderr, "Got error from krb5_init_via_keytab %d\n", status);
        webauth_krb5_free(ctxt);
        return 0;
    }
    
    status = webauth_krb5_mk_req(ctxt, kdc_princ, k5_req, k5_req_len);
    if(status != WA_ERR_NONE) {
        // TODO: log error
        fprintf(stderr, "Got error from krb5_mk_req %d\n", status);
        webauth_krb5_free(ctxt);
        return 0;
    }
    
    webauth_krb5_free(ctxt);
    
    return 1;
}

static int process_session_key_req(ETERM *data, ETERM **ret) {
    int k5_req_len, retval = 0;
    char *k5_req, *keytab, *kdc_princ;
    ETERM *result, *ekt, *ekdcprinc;
    
    assert(ERL_IS_TUPLE(data));
    assert(ERL_TUPLE_SIZE(data) == 2);
    
    ekt = erl_element(1, data);
    ekdcprinc = erl_element(2, data);
    
    assert(ERL_IS_LIST(ekt));
    assert(ERL_IS_LIST(ekdcprinc));
    
    keytab = erl_iolist_to_string(ekt);
    kdc_princ = erl_iolist_to_string(ekdcprinc);
        
    retval = session_key_req(keytab, kdc_princ, &k5_req, &k5_req_len);
    erl_free(keytab);
    erl_free(kdc_princ);
    
    if(retval) {
        fprintf(stderr, "Got req string length: %d\n", k5_req_len);
        *ret = erl_mk_binary(k5_req, k5_req_len);
    }
    
    return retval;
}

static int write_buf(int fd, uint8_t *buf, int len) {
    int l, done = 0;
    
    do {
        if((l = write(fd, buf + done, len - done)) < 0) {
            if(errno != EINTR) break;
        } else
            done += l;
    } while(done < len);
    
    return done;
}

static int write_cmd(uint8_t *buf, int len) {
    uint8_t hd[2];
    enc_int16(len, hd);
    
    if(write_buf(1, hd, 2) != 2)
        return 0;
    if(write_buf(1, buf, len) != len)
        return 0;
        
    return 1;
}

static int process_command(uint8_t *buf) {
    int len, retval = 0;
    ETERM *pattern, *tuple, *cmd, *port, *data, *ret = NULL, *result;
    char *cmdstr;
    uint8_t *outbuf;
    
    pattern = erl_format("{Cmd, Port, Data}");
    tuple = erl_decode(buf);
    
    if(erl_match(pattern, tuple)) {
        cmd = erl_var_content(pattern, "Cmd");
        port = erl_var_content(pattern, "Port");
        data = erl_var_content(pattern, "Data");
        
        cmdstr = ERL_ATOM_PTR(cmd);
        fprintf(stderr, "Got command: %s\n", cmdstr);
        if(!strcmp(cmdstr, "session_key_req")) {
            retval = process_session_key_req(data, &ret);
        }
        
        if(retval && ret) {
            result = erl_format("{~w, ~w, {ok, ~w}}", cmd, port, ret);
        } else if(retval) {
            result = erl_format("{~w, ~w, ok}", cmd, port);
        } else {
            result = erl_format("{~w, ~w, {error, ewebauth}}", cmd, port);
        }
        
        len = erl_term_len(result);
        outbuf = erl_malloc(len);
        erl_encode(result, outbuf);
        
        retval = write_cmd(outbuf, len) && retval;
        
        erl_free(outbuf);
        
        erl_free_term(result);
        erl_free_term(cmd);
        erl_free_term(port);
        erl_free_term(data);
        if(ret != NULL)
            erl_free_term(ret);
    }
    else fprintf(stderr, "Invalid input.\n");
    
    erl_free_term(pattern);
    erl_free_term(tuple);
    
    return retval;
}

static int read_buf(int fd, uint8_t *buf, int len) {
    int l, got = 0;
    
    do {
        if((l = read(fd, buf + got, len - got)) <= 0) {
            if(l == 0) break;
            if(errno != EINTR) break;
        } else
            got += l;
    } while(got < len);
    
    return got;
}

static int read_cmd(uint8_t *buf) {
    int len = 0;
    
    // Read Len
    if(read_buf(0, buf, 2) != 2)
        return 0;
    
    // Read Data
    len = ntohs(((uint16_t*)buf)[0]);
    if(read_buf(0, buf, len) != len)
        return 0;
    
    return 1;
}

int main(int argc, char **argv) {
    erl_init(NULL, 0);
    uint8_t buf[BUFSIZE];
    int retval = 0;
        
    do {
        if(read_cmd(buf) > 0)
            retval = process_command(buf);
        else
            break;
    } while(retval);
    
    return 0;
}
